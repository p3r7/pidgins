
-- ------------------------------------------------------------------------
-- FENNEL

function unrequire(m)
  package.loaded[m] = nil
  -- _G[m] = nil
end

local fennel = include('lib/fennel-1.0.0/fennel')
table.insert(package.loaders or package.searchers, fennel.searcher)

local fnl_path = "/home/we/dust/code/pidgins/lib/test"
unrequire(fnl_path) -- NB: unrequire to force recompile
local mylib = require(fnl_path)


-- ------------------------------------------------------------------------
-- MAIN

function redraw()
  mylib.redraw()
end

function init()
  mylib.init()
end
