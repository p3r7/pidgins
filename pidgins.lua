
-- ------------------------------------------------------------------------
-- FENNEL

local n_fennel = include('lib/norns_fennel')
fennel = n_fennel.load_fennel()

local pidgins = require("lib/pidgins")
pprint = pidgins.pprint


-- ------------------------------------------------------------------------
-- MAIN

function redraw()
  pidgins.redraw()
end

function init()
  pidgins.init()
end

function key(id, state)
  pidgins.key(id, state)
end

function enc(id, delta)
  pidgins.enc(id, delta)
end
