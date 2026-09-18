local M = {}
M.data = source()
function M.leak(obj)
  -- ruleid: taint_lua_colon_call_dot_definition
  sink(obj.data)
end
M:leak()
