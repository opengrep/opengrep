local M = {}
function M.f(x)
  -- ruleid: taint_lua_dotted_definition_call_in_function
  sink(x)
end
function g()
  M.f(source())
end
g()
