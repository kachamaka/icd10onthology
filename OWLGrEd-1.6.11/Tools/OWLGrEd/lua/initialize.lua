function eval(string_to_eval)
  log("---> eval called, got length", #string_to_eval)
  return loadstring("return " .. string_to_eval)()
end
--log = function () end

log = function () end

lQuery('Project'):attr('log_translet', '')
