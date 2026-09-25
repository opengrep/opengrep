-- A method defined with a colon receives the object as its first
-- parameter, so a colon call on self inside it reaches the object's method.
local M = {}
function M:store(v)
  -- ruleid: taint_lua_colon_definition_receiver
  sink(v)
end
function M:run(x)
  self:store(x)
end
M:run(source())
