local M = {}
function M:f()
  -- ruleid: taint_lua_colon_definition_call_in_function
  sink(self.data)
end
function g()
  M.data = source()
  M:f()
end
g()
