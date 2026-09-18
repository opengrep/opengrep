local M = {}
M.f = function(x)
  -- ruleid: taint_lua_assigned_field_call_in_function
  sink(x)
end
function g()
  M.f(source())
end
g()
