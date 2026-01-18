package main

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

const BaseURI = "http://example.org/icd10#"

// -------------------- INPUT MODEL --------------------
type ICDRecord struct {
	ICD10Code string `json:"icd10code"`
	Type      string `json:"type"` // "category" or "subcategory"
	Title     string `json:"title"`

	Chapter     string `json:"chapter"`
	ChapterCode string `json:"chapterCode"`

	Block     string `json:"block"`
	BlockCode string `json:"blockCode"`

	Category     string `json:"category"`
	CategoryCode string `json:"categoryCode"`

	Subcategory string `json:"subcategory"`

	Inclusion []string `json:"inclusion"`
	Exclusion []string `json:"exclusion"`
	Symptoms  []string `json:"symptoms"`
}

// -------------------- RDF/XML WRITER --------------------
type RDFWriter struct {
	enc *xml.Encoder
}

func NewRDFWriter(f *os.File) *RDFWriter {
	enc := xml.NewEncoder(f)
	enc.Indent("", "  ")
	return &RDFWriter{enc: enc}
}

func (rw *RDFWriter) StartRDF() error {
	start := xml.StartElement{
		Name: xml.Name{Local: "rdf:RDF"},
		Attr: []xml.Attr{
			{Name: xml.Name{Local: "xmlns:rdf"}, Value: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"},
			{Name: xml.Name{Local: "xmlns:rdfs"}, Value: "http://www.w3.org/2000/01/rdf-schema#"},
			{Name: xml.Name{Local: "xmlns:owl"}, Value: "http://www.w3.org/2002/07/owl#"},
			{Name: xml.Name{Local: "xmlns:xsd"}, Value: "http://www.w3.org/2001/XMLSchema#"},
			{Name: xml.Name{Local: "xmlns:icd"}, Value: BaseURI},
		},
	}
	return rw.enc.EncodeToken(start)
}

func (rw *RDFWriter) EndRDF() error {
	if err := rw.enc.EncodeToken(xml.EndElement{Name: xml.Name{Local: "rdf:RDF"}}); err != nil {
		return err
	}
	return rw.enc.Flush()
}

func (rw *RDFWriter) WriteEmptyElement(qname string, attrs ...xml.Attr) error {
	se := xml.StartElement{Name: xml.Name{Local: qname}, Attr: attrs}
	if err := rw.enc.EncodeToken(se); err != nil {
		return err
	}
	return rw.enc.EncodeToken(xml.EndElement{Name: se.Name})
}

func (rw *RDFWriter) WriteOntology() error {
	return rw.WriteEmptyElement("owl:Ontology", xml.Attr{Name: xml.Name{Local: "rdf:about"}, Value: BaseURI})
}

func (rw *RDFWriter) WriteClass(local string) error {
	return rw.WriteEmptyElement("owl:Class", xml.Attr{Name: xml.Name{Local: "rdf:about"}, Value: classURI(local)})
}

func (rw *RDFWriter) WriteObjectProperty(local string, domainLocals []string, rangeLocals []string) error {
	start := xml.StartElement{Name: xml.Name{Local: "owl:ObjectProperty"}, Attr: []xml.Attr{
		{Name: xml.Name{Local: "rdf:about"}, Value: classURI(local)},
	}}
	if err := rw.enc.EncodeToken(start); err != nil {
		return err
	}
	for _, d := range domainLocals {
		if err := rw.WriteEmptyElement("rdfs:domain", xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: classURI(d)}); err != nil {
			return err
		}
	}
	for _, r := range rangeLocals {
		if err := rw.WriteEmptyElement("rdfs:range", xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: classURI(r)}); err != nil {
			return err
		}
	}
	return rw.enc.EncodeToken(xml.EndElement{Name: start.Name})
}

func (rw *RDFWriter) WriteDatatypeProperty(local string, functional bool, domainLocals []string) error {
	start := xml.StartElement{Name: xml.Name{Local: "owl:DatatypeProperty"}, Attr: []xml.Attr{
		{Name: xml.Name{Local: "rdf:about"}, Value: classURI(local)},
	}}
	if err := rw.enc.EncodeToken(start); err != nil {
		return err
	}
	for _, d := range domainLocals {
		if err := rw.WriteEmptyElement("rdfs:domain", xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: classURI(d)}); err != nil {
			return err
		}
	}

	// NOTE: "..."@en literals are rdf:langString, not xsd:string.
	// If the range is forced to xsd:string, reasoners (e.g., HermiT) will mark the ontology inconsistent.
	rangeIRI := "http://www.w3.org/2001/XMLSchema#string"
	if local == "title" || local == "symptomLabel" {
		rangeIRI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
	}

	if err := rw.WriteEmptyElement("rdfs:range", xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: rangeIRI}); err != nil {
		return err
	}

	if functional {
		if err := rw.WriteEmptyElement("rdf:type", xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: "http://www.w3.org/2002/07/owl#FunctionalProperty"}); err != nil {
			return err
		}
	}
	return rw.enc.EncodeToken(xml.EndElement{Name: start.Name})
}

type Node struct {
	About   string // URI for named resources
	NodeID  string // rdf:nodeID for blank nodes (mutually exclusive with About)
	Triples []Triple
}
type Triple struct {
	PredQName string
	Object    string
	IsIRI     bool
	IsNodeID  bool
	Lang      string
}

func (rw *RDFWriter) WriteNode(n Node) error {
	attrs := []xml.Attr{}
	if n.NodeID != "" {
		attrs = append(attrs, xml.Attr{Name: xml.Name{Local: "rdf:nodeID"}, Value: n.NodeID})
	} else {
		attrs = append(attrs, xml.Attr{Name: xml.Name{Local: "rdf:about"}, Value: n.About})
	}
	start := xml.StartElement{Name: xml.Name{Local: "rdf:Description"}, Attr: attrs}
	if err := rw.enc.EncodeToken(start); err != nil {
		return err
	}
	for _, t := range n.Triples {
		el := xml.StartElement{Name: xml.Name{Local: t.PredQName}}
		if t.IsIRI {
			el.Attr = append(el.Attr, xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: t.Object})
			if err := rw.enc.EncodeToken(el); err != nil {
				return err
			}
			if err := rw.enc.EncodeToken(xml.EndElement{Name: el.Name}); err != nil {
				return err
			}
			continue
		}
		if t.IsNodeID {
			el.Attr = append(el.Attr, xml.Attr{Name: xml.Name{Local: "rdf:nodeID"}, Value: t.Object})
			if err := rw.enc.EncodeToken(el); err != nil {
				return err
			}
			if err := rw.enc.EncodeToken(xml.EndElement{Name: el.Name}); err != nil {
				return err
			}
			continue
		}
		if t.Lang != "" {
			el.Attr = append(el.Attr, xml.Attr{Name: xml.Name{Local: "xml:lang"}, Value: t.Lang})
		}
		if err := rw.enc.EncodeToken(el); err != nil {
			return err
		}
		if err := rw.enc.EncodeToken(xml.CharData([]byte(t.Object))); err != nil {
			return err
		}
		if err := rw.enc.EncodeToken(xml.EndElement{Name: el.Name}); err != nil {
			return err
		}
	}
	if err := rw.enc.EncodeToken(xml.EndElement{Name: start.Name}); err != nil {
		return err
	}
	return nil
}

func (rw *RDFWriter) WriteTransitiveObjectProperty(local string) error {
	start := xml.StartElement{Name: xml.Name{Local: "owl:ObjectProperty"}, Attr: []xml.Attr{
		{Name: xml.Name{Local: "rdf:about"}, Value: classURI(local)},
	}}
	if err := rw.enc.EncodeToken(start); err != nil {
		return err
	}

	if err := rw.WriteEmptyElement(
		"rdf:type",
		xml.Attr{Name: xml.Name{Local: "rdf:resource"}, Value: "http://www.w3.org/2002/07/owl#TransitiveProperty"},
	); err != nil {
		return err
	}

	return rw.enc.EncodeToken(xml.EndElement{Name: start.Name})
}

// -------------------- HELPERS --------------------
func uri(fragment string) string { // individuals by code, chapterCode, blockCode
	return BaseURI + strings.TrimSpace(fragment)
}
func classURI(local string) string { // classes & properties
	return BaseURI + strings.TrimSpace(local)
}

var nonAlnum = regexp.MustCompile(`[^A-Za-z0-9_]+`)
var multiUnderscore = regexp.MustCompile(`_+`)

func safeLocalNameForConcept(s string) string {
	s = strings.TrimSpace(s)
	s = strings.ToLower(s)
	s = strings.ReplaceAll(s, " ", "_")
	s = strings.ReplaceAll(s, ".", "_")
	s = strings.ReplaceAll(s, "-", "_")
	s = nonAlnum.ReplaceAllString(s, "_")
	s = multiUnderscore.ReplaceAllString(s, "_")
	s = strings.Trim(s, "_")
	if s == "" {
		return "x"
	}
	return s
}

func addOnce(set map[string]bool, key string) bool {
	if set[key] {
		return false
	}
	set[key] = true
	return true
}

// Read all *.json in one folder (non-recursive)
func readAllJSON(dir string) ([]ICDRecord, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}
	var recs []ICDRecord
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if !strings.HasSuffix(strings.ToLower(e.Name()), ".json") {
			continue
		}
		path := filepath.Join(dir, e.Name())
		b, err := os.ReadFile(path)
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", e.Name(), err)
		}
		b = bytes.TrimSpace(b)
		if len(b) == 0 {
			continue
		}
		var r ICDRecord
		if err := json.Unmarshal(b, &r); err != nil {
			return nil, fmt.Errorf("json parse %s: %w", e.Name(), err)
		}
		recs = append(recs, r)
	}
	return recs, nil
}

// -------------------- RULES: BodySystem & Etiology --------------------

// BodySystem only for anatomically-defined chapters (VI–XIV)
var chapterToBodySystem = map[string]string{
	"VI":   "Nervous system",
	"VII":  "Eye and adnexa",
	"VIII": "Ear and mastoid process",
	"IX":   "Circulatory system",
	"X":    "Respiratory system",
	"XI":   "Digestive system",
	"XII":  "Skin and subcutaneous tissue",
	"XIII": "Musculoskeletal system and connective tissue",
	"XIV":  "Genitourinary system",
}

func inferBodySystemFromChapter(chapterCode string) string {
	ch := strings.ToUpper(strings.TrimSpace(chapterCode))
	if bs, ok := chapterToBodySystem[ch]; ok {
		return bs
	}
	return ""
}

// Etiology (causative agent) only if explicitly extractable from title
var reDueTo = regexp.MustCompile(`(?i)\b(?:due to|caused by)\s+(.+)$`)
var reWithInfection = regexp.MustCompile(`(?i)\bwith\s+(.+?)\s+infection\b`)
var reHIV = regexp.MustCompile(`(?i)\bhuman immunodeficiency virus\b|\bHIV\b`)

func extractEtiologyFromTitle(title string) string {
	t := strings.TrimSpace(title)
	if t == "" {
		return ""
	}
	if m := reDueTo.FindStringSubmatch(t); len(m) == 2 {
		agent := strings.TrimSpace(m[1])
		agent = strings.TrimRight(agent, " .;")
		return agent
	}
	if reHIV.MatchString(t) {
		return "Human immunodeficiency virus (HIV)"
	}
	if m := reWithInfection.FindStringSubmatch(t); len(m) == 2 {
		agent := strings.TrimSpace(m[1])
		agent = strings.TrimRight(agent, " .;")
		return agent
	}
	return ""
}

// -------------------- MAIN BUILD --------------------
func main() {
	inDir := flag.String("dir", "", "Directory containing *.json files (non-recursive)")
	outPath := flag.String("out", "icd10_full.owl", "Output OWL file (.owl, RDF/XML)")
	flag.Parse()

	if *inDir == "" {
		fmt.Fprintln(os.Stderr, "Missing -dir")
		os.Exit(1)
	}

	records, err := readAllJSON(*inDir)
	if err != nil {
		fmt.Fprintln(os.Stderr, "Input error:", err)
		os.Exit(1)
	}
	// Stable order for reproducibility
	sort.Slice(records, func(i, j int) bool {
		return records[i].ICD10Code < records[j].ICD10Code
	})

	// --------------------
	subset := make([]ICDRecord, 0, len(records)/5)
	for i, r := range records {
		if i%20 == 0 { // take every 20th record ≈ 5%
			subset = append(subset, r)
		}
	}

	// Replace full dataset with subset
	records = subset

	fmt.Printf("Using subset: %d records (~5%%)\n", len(records))
	// --------------------

	nodes := map[string]*Node{}
	getNode := func(about string) *Node {
		if n, ok := nodes[about]; ok {
			return n
		}
		n := &Node{About: about}
		nodes[about] = n
		return n
	}

	// Blank nodes (rdf:nodeID)
	bnodeCounter := 0
	newBNodeID := func() string {
		bnodeCounter++
		return fmt.Sprintf("N%06d", bnodeCounter)
	}
	getBNode := func(nodeID string) *Node {
		key := "_:" + nodeID
		if n, ok := nodes[key]; ok {
			return n
		}
		n := &Node{NodeID: nodeID}
		nodes[key] = n
		return n
	}

	addType := func(n *Node, classLocal string) {
		n.Triples = append(n.Triples, Triple{
			PredQName: "rdf:type",
			Object:    classURI(classLocal),
			IsIRI:     true,
		})
	}
	addTypeIRI := func(n *Node, iri string) {
		n.Triples = append(n.Triples, Triple{
			PredQName: "rdf:type",
			Object:    iri,
			IsIRI:     true,
		})
	}
	addLit := func(n *Node, predLocal, val, lang string) {
		val = strings.TrimSpace(val)
		if val == "" {
			return
		}
		n.Triples = append(n.Triples, Triple{
			PredQName: "icd:" + predLocal,
			Object:    val,
			IsIRI:     false,
			Lang:      lang,
		})
	}
	addComment := func(n *Node, val string) {
		val = strings.TrimSpace(val)
		if val == "" {
			return
		}
		n.Triples = append(n.Triples, Triple{
			PredQName: "rdfs:comment",
			Object:    val,
			IsIRI:     false,
			Lang:      "en",
		})
	}
	addObj := func(n *Node, propLocal string, objURI string) {
		n.Triples = append(n.Triples, Triple{
			PredQName: "icd:" + propLocal,
			Object:    objURI,
			IsIRI:     true,
		})
	}
	addIRI := func(n *Node, predQName string, objURI string) {
		n.Triples = append(n.Triples, Triple{
			PredQName: predQName,
			Object:    objURI,
			IsIRI:     true,
		})
	}
	addNodeIDRef := func(n *Node, predQName string, nodeID string) {
		n.Triples = append(n.Triples, Triple{
			PredQName: predQName,
			Object:    nodeID,
			IsNodeID:  true,
		})
	}

	// Dedup structural links
	hasBlockSet := map[string]bool{}
	hasCategorySet := map[string]bool{}
	hasSubcategorySet := map[string]bool{}

	// Entity factories:
	// NOTE: Individuals use uri(code/chapterCode/blockCode)
	getChapter := func(code, title string) string {
		about := uri(code) // e.g. ...#I
		n := getNode(about)
		addType(n, "Chapter")
		addLit(n, "chapterCode", code, "")
		addLit(n, "title", title, "en")
		return about
	}
	getBlock := func(code, title string) string {
		about := uri(code) // e.g. ...#A00-A09
		n := getNode(about)
		addType(n, "Block")
		addLit(n, "blockCode", code, "")
		addLit(n, "title", title, "en")
		return about
	}
	getCategory := func(code, title string) string {
		about := uri(code) // e.g. ...#A06
		n := getNode(about)
		addType(n, "Category")
		addLit(n, "codeValue", code, "")
		addLit(n, "categoryCode", code, "")
		addLit(n, "title", title, "en")
		return about
	}
	getSubcategory := func(code, title string) string {
		about := uri(code) // e.g. ...#A06.6
		n := getNode(about)
		addType(n, "Subcategory")
		addLit(n, "codeValue", code, "")
		addLit(n, "title", title, "en")
		return about
	}

	// Concept factories (Symptom, BodySystem, EtiologyFactor) must have safe local names
	getSymptom := func(name string) string {
		about := classURI(safeLocalNameForConcept(name))
		n := getNode(about)
		addType(n, "Symptom")
		addLit(n, "symptomLabel", name, "en")
		return about
	}

	getBodySystem := func(name string) string {
		about := classURI(safeLocalNameForConcept(name))
		n := getNode(about)
		addType(n, "BodySystem")
		addLit(n, "title", name, "en")
		return about
	}
	getEtiology := func(name string) string {
		about := classURI(safeLocalNameForConcept(name))
		n := getNode(about)
		addType(n, "EtiologyFactor")
		addLit(n, "title", name, "en")
		return about
	}

	// Process records
	for _, r := range records {
		typ := strings.ToLower(strings.TrimSpace(r.Type))
		if typ != "category" && typ != "subcategory" {
			continue
		}

		// Main entity uses exact code as fragment
		var entURI string
		if typ == "category" {
			code := strings.TrimSpace(r.CategoryCode)
			if code == "" {
				code = strings.TrimSpace(r.ICD10Code)
			}
			if code == "" {
				continue
			}
			title := strings.TrimSpace(r.Title)
			if title == "" {
				title = strings.TrimSpace(r.Category)
			}
			entURI = getCategory(code, title)
		} else {
			code := strings.TrimSpace(r.ICD10Code)
			if code == "" {
				code = strings.TrimSpace(r.Subcategory)
			}
			if code == "" {
				continue
			}
			entURI = getSubcategory(code, r.Title)
		}
		entNode := getNode(entURI)

		// Structure: Chapter/Block/Category
		var chURI, blURI, catURI string

		if strings.TrimSpace(r.ChapterCode) != "" {
			chURI = getChapter(r.ChapterCode, r.Chapter)
			addObj(entNode, "belongsToChapter", chURI)
		}
		if strings.TrimSpace(r.BlockCode) != "" {
			blURI = getBlock(r.BlockCode, r.Block)
			addObj(entNode, "belongsToBlock", blURI)
			if chURI != "" {
				key := chURI + "->" + blURI
				if addOnce(hasBlockSet, key) {
					chNode := getNode(chURI)
					addObj(chNode, "hasBlock", blURI)
				}
			}
		}
		if strings.TrimSpace(r.CategoryCode) != "" {
			catURI = getCategory(r.CategoryCode, r.Category)
			if blURI != "" {
				key := blURI + "->" + catURI
				if addOnce(hasCategorySet, key) {
					blNode := getNode(blURI)
					addObj(blNode, "hasCategory", catURI)
				}
			}
		}
		if typ == "subcategory" && catURI != "" {
			addObj(entNode, "belongsToCategory", catURI)
			key := catURI + "->" + entURI
			if addOnce(hasSubcategorySet, key) {
				cNode := getNode(catURI)
				addObj(cNode, "hasSubcategory", entURI)
			}
		}

		// Symptoms
		for _, s := range r.Symptoms {
			s = strings.TrimSpace(s)
			if s == "" {
				continue
			}
			addObj(entNode, "hasSymptom", getSymptom(s))
		}

		// Enrichment: BodySystem (only VI–XIV)
		if bs := inferBodySystemFromChapter(r.ChapterCode); bs != "" {
			addObj(entNode, "affectsBodySystem", getBodySystem(bs))
		}

		// Enrichment: Etiology (only explicit causative agent from title)
		if agent := extractEtiologyFromTitle(r.Title); agent != "" {
			addObj(entNode, "hasEtiology", getEtiology(agent))
		}

		// Inclusion/Exclusion as rdfs:comment
		var notes []string
		if len(r.Inclusion) > 0 {
			notes = append(notes, "INCLUDES: "+strings.Join(r.Inclusion, "; "))
		}
		if len(r.Exclusion) > 0 {
			notes = append(notes, "EXCLUDES: "+strings.Join(r.Exclusion, "; "))
		}
		if len(notes) > 0 {
			addComment(entNode, strings.Join(notes, "\n"))
		}
	}

	// --------------------
	// Extra OWL axioms (Type A enrichment)
	// --------------------
	// Ensure key individuals exist for hasValue restrictions
	respSys := getBodySystem("Respiratory system")
	chI := getChapter("I", "Certain infectious and parasitic diseases")
	chII := getChapter("II", "Neoplasms")
	chXIX := getChapter("XIX", "Injury, poisoning and certain other consequences of external causes")

	ensureClass := func(local string) *Node {
		n := getNode(classURI(local))
		addTypeIRI(n, "http://www.w3.org/2002/07/owl#Class")
		return n
	}
	ensureObjProp := func(local string) *Node {
		n := getNode(classURI(local))
		addTypeIRI(n, "http://www.w3.org/2002/07/owl#ObjectProperty")
		return n
	}

	// Base top-level disease class
	ensureClass("Disease")

	// Category/Subcategory are kinds of Disease
	addIRI(ensureClass("Category"), "rdfs:subClassOf", classURI("Disease"))
	addIRI(ensureClass("Subcategory"), "rdfs:subClassOf", classURI("Disease"))

	// Helper: build (Disease AND Restriction(...)) expression as a blank-node class
	buildIntersectionWithRestriction := func(onPropLocal string, restrictionPredQName string, restrictionObj string, restrictionObjIsIRI bool) string {
		exprID := newBNodeID()
		expr := getBNode(exprID)
		addTypeIRI(expr, "http://www.w3.org/2002/07/owl#Class")

		list1ID := newBNodeID()
		list2ID := newBNodeID()
		addNodeIDRef(expr, "owl:intersectionOf", list1ID)

		// Restriction node
		restID := newBNodeID()
		rest := getBNode(restID)
		addTypeIRI(rest, "http://www.w3.org/2002/07/owl#Restriction")
		addIRI(rest, "owl:onProperty", classURI(onPropLocal))
		if restrictionObjIsIRI {
			addIRI(rest, restrictionPredQName, restrictionObj)
		} else {
			rest.Triples = append(rest.Triples, Triple{PredQName: restrictionPredQName, Object: restrictionObj})
		}

		// rdf:List: ( Disease  Restriction )
		l1 := getBNode(list1ID)
		addIRI(l1, "rdf:first", classURI("Disease"))
		addNodeIDRef(l1, "rdf:rest", list2ID)
		l2 := getBNode(list2ID)
		addIRI(l2, "rdf:first", "_:"+restID) // placeholder, patched below
		// Fix: rdf:first must point to blank node via rdf:nodeID, not rdf:resource
		l2.Triples = l2.Triples[:len(l2.Triples)-1]
		addNodeIDRef(l2, "rdf:first", restID)
		addIRI(l2, "rdf:rest", "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")

		return exprID
	}

	// Helper: build Restriction-only class expression (e.g., hasSymptom some Symptom)
	buildRestrictionClass := func(onPropLocal string, restrictionPredQName string, restrictionObjIRI string) string {
		exprID := newBNodeID()
		expr := getBNode(exprID)
		addTypeIRI(expr, "http://www.w3.org/2002/07/owl#Class")
		restID := newBNodeID()
		rest := getBNode(restID)
		addTypeIRI(rest, "http://www.w3.org/2002/07/owl#Restriction")
		addIRI(rest, "owl:onProperty", classURI(onPropLocal))
		addIRI(rest, restrictionPredQName, restrictionObjIRI)
		addNodeIDRef(expr, "owl:intersectionOf", newBNodeID()) // placeholder list
		// Replace placeholder with list of (Disease rest)
		listID := expr.Triples[len(expr.Triples)-1].Object
		l1 := getBNode(listID)
		addIRI(l1, "rdf:first", classURI("Disease"))
		l2ID := newBNodeID()
		addNodeIDRef(l1, "rdf:rest", l2ID)
		l2 := getBNode(l2ID)
		addNodeIDRef(l2, "rdf:first", restID)
		addIRI(l2, "rdf:rest", "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
		return exprID
	}

	// RespiratoryDisease ≡ Disease AND (affectsBodySystem value respiratory_system)
	ensureClass("RespiratoryDisease")
	rdExpr := buildIntersectionWithRestriction("affectsBodySystem", "owl:hasValue", respSys, true)
	addNodeIDRef(ensureClass("RespiratoryDisease"), "owl:equivalentClass", rdExpr)

	// InfectiousDisease ≡ Disease AND (belongsToChapter value I)
	ensureClass("InfectiousDisease")
	idExpr := buildIntersectionWithRestriction("belongsToChapter", "owl:hasValue", chI, true)
	addNodeIDRef(ensureClass("InfectiousDisease"), "owl:equivalentClass", idExpr)

	// NeoplasmDisease ≡ Disease AND (belongsToChapter value II)
	ensureClass("NeoplasmDisease")
	ndExpr := buildIntersectionWithRestriction("belongsToChapter", "owl:hasValue", chII, true)
	addNodeIDRef(ensureClass("NeoplasmDisease"), "owl:equivalentClass", ndExpr)

	// InjuryDisease ≡ Disease AND (belongsToChapter value XIX)
	ensureClass("InjuryDisease")
	injExpr := buildIntersectionWithRestriction("belongsToChapter", "owl:hasValue", chXIX, true)
	addNodeIDRef(ensureClass("InjuryDisease"), "owl:equivalentClass", injExpr)

	// SymptomaticDisease ≡ Disease AND (hasSymptom some Symptom)
	ensureClass("SymptomaticDisease")
	symExpr := buildRestrictionClass("hasSymptom", "owl:someValuesFrom", classURI("Symptom"))
	addNodeIDRef(ensureClass("SymptomaticDisease"), "owl:equivalentClass", symExpr)

	// Inverse properties
	inversePairs := [][2]string{
		{"hasBlock", "blockOfChapter"},
		{"hasCategory", "categoryOfBlock"},
		{"hasSubcategory", "subcategoryOfCategory"},
		{"affectsBodySystem", "bodySystemAffectedBy"},
	}
	for _, p := range inversePairs {
		a := ensureObjProp(p[0])
		b := ensureObjProp(p[1])
		addIRI(a, "owl:inverseOf", classURI(p[1]))
		addIRI(b, "owl:inverseOf", classURI(p[0]))
	}

	// Transitive ancestor relation and sub-property
	ancestor := ensureObjProp("hasAncestorCategory")
	addTypeIRI(ancestor, "http://www.w3.org/2002/07/owl#TransitiveProperty")
	addIRI(ancestor, "rdfs:domain", classURI("Subcategory"))
	addIRI(ancestor, "rdfs:range", classURI("Category"))
	belongs := ensureObjProp("belongsToCategory")
	addIRI(belongs, "rdfs:subPropertyOf", classURI("hasAncestorCategory"))

	// Write OWL
	out, err := os.Create(*outPath)
	if err != nil {
		fmt.Fprintln(os.Stderr, "Output error:", err)
		os.Exit(1)
	}
	defer out.Close()

	rw := NewRDFWriter(out)
	if err := rw.StartRDF(); err != nil {
		fmt.Fprintln(os.Stderr, "XML error:", err)
		os.Exit(1)
	}

	_ = rw.WriteOntology()

	// Classes (base + enriched)
	classLocals := []string{
		"Chapter", "Block", "Category", "Subcategory",
		"Symptom", "EtiologyFactor", "BodySystem",
		"Disease", "RespiratoryDisease", "InfectiousDisease", "NeoplasmDisease", "InjuryDisease", "SymptomaticDisease",
	}
	for _, c := range classLocals {
		_ = rw.WriteClass(c)
	}

	// Object properties
	_ = rw.WriteObjectProperty("hasBlock", []string{"Chapter"}, []string{"Block"})
	_ = rw.WriteObjectProperty("blockOfChapter", []string{"Block"}, []string{"Chapter"})
	_ = rw.WriteObjectProperty("hasCategory", []string{"Block"}, []string{"Category"})
	_ = rw.WriteObjectProperty("categoryOfBlock", []string{"Category"}, []string{"Block"})
	_ = rw.WriteObjectProperty("hasSubcategory", []string{"Category"}, []string{"Subcategory"})
	_ = rw.WriteObjectProperty("subcategoryOfCategory", []string{"Subcategory"}, []string{"Category"})
	_ = rw.WriteObjectProperty("belongsToChapter", []string{"Category", "Subcategory"}, []string{"Chapter"})
	_ = rw.WriteObjectProperty("belongsToBlock", []string{"Category", "Subcategory"}, []string{"Block"})
	_ = rw.WriteObjectProperty("belongsToCategory", []string{"Subcategory"}, []string{"Category"})
	_ = rw.WriteObjectProperty("hasAncestorCategory", []string{"Subcategory"}, []string{"Category"})
	_ = rw.WriteObjectProperty("hasSymptom", []string{"Category", "Subcategory"}, []string{"Symptom"})
	_ = rw.WriteObjectProperty("hasEtiology", []string{"Category", "Subcategory"}, []string{"EtiologyFactor"})
	_ = rw.WriteObjectProperty("affectsBodySystem", []string{"Category", "Subcategory"}, []string{"BodySystem"})
	_ = rw.WriteObjectProperty("bodySystemAffectedBy", []string{"BodySystem"}, []string{"Category", "Subcategory"})

	// Datatype properties
	_ = rw.WriteDatatypeProperty("codeValue", true, []string{"Category", "Subcategory"})
	_ = rw.WriteDatatypeProperty("title", true, []string{"Chapter", "Block", "Category", "Subcategory", "EtiologyFactor", "BodySystem"})
	_ = rw.WriteDatatypeProperty("chapterCode", true, []string{"Chapter"})
	_ = rw.WriteDatatypeProperty("blockCode", true, []string{"Block"})
	_ = rw.WriteDatatypeProperty("categoryCode", true, []string{"Category"})
	_ = rw.WriteDatatypeProperty("symptomLabel", false, []string{"Symptom"})

	// Individuals (stable order)
	keys := make([]string, 0, len(nodes))
	for k := range nodes {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		if err := rw.WriteNode(*nodes[k]); err != nil {
			fmt.Fprintln(os.Stderr, "XML error:", err)
			os.Exit(1)
		}
	}

	if err := rw.EndRDF(); err != nil {
		fmt.Fprintln(os.Stderr, "XML error:", err)
		os.Exit(1)
	}

	fmt.Println("Saved:", *outPath)
}
