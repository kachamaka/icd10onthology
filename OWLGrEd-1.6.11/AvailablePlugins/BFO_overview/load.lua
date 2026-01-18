require("lQuery")
local completeMetamodelUserFields = require "OWLGrEd_UserFields.completeMetamodel"

local path

if tda.isWeb then 
	path = tda.FindPath(tda.GetToolPath() .. "/AllPlugins", "BFO_overview") .. "/"
else
	path = tda.GetProjectPath() .. "\\Plugins\\BFO_overview\\"
end

local pathContextType = path .. "AutoLoad"
completeMetamodelUserFields.loadAutoLoadProfiles(pathContextType)

return true

