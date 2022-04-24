

local norns_fennel = {}


-- ------------------------------------------------------------------------
-- UNLOAD MODULES

-- FIXME: really unprecise way of unloading required libs.
-- indeed, if a loaded module has the same name as a non-loaded yet existing module in current script, can wreak havoc.
-- alternative would be to have a wrapped `fennel.searcher` that'd register loaded fenel modules in a table under _G
-- TODO: maybe use fennel.searchModule?
norns_fennel.unrequireAllScriptLib = function ()
  for path, _ in pairs(package.loaded) do
    local p = norns.state.path..path..'.fnl'
    if util.file_exists(p) then
      print("unloading "..p)
      package.loaded[path] = nil
    end
  end
end

norns_fennel.unrequireAllMacros = function (fennel)
  for path, _ in pairs(fennel['macroLoaded']) do
    fennel['macroLoaded'][path] = nil
  end
end


-- ------------------------------------------------------------------------
-- LOAD FENNEL

norns_fennel.load_fennel = function ()
  local fennel = include('lib/fennel/fennel')

  -- NB: matches `include` beahviour
  local dirs = {norns.state.path, _path.code, _path.extn}
  for _, dir in ipairs(dirs) do
    fennel.path = fennel.path .. ";".. dir.."?.fnl"
    fennel.path = fennel.path .. ";".. dir.."?/init.fnl"
    fennel['macro-path'] = fennel['macro-path'] .. ";".. dir.."?.fnl"
    fennel['macro-path'] = fennel['macro-path'] .. ";".. dir.."?/init-macros.fnl"
    fennel['macro-path'] = fennel['macro-path'] .. ";".. dir.."?/init.fnl"
  end

  --NB: for deps that require fennel
  fennel.path = fennel.path .. ";".. norns.state.path.."lib/fennel/?.fnl"

  if package.loaded['fennel.repl'] ~= nil then
    table.insert(package.loaders or package.searchers, fennel.searcher)
  end

  -- NB: these are require for norns as the Lua env doesn't get destroyed between script launches
  norns_fennel.unrequireAllScriptLib()
  norns_fennel.unrequireAllMacros(fennel)

  return fennel
end



-- ------------------------------------------------------------------------

return norns_fennel
