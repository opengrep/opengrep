local M = {}
M.data = source()
function M:leak()
  -- ruleid: taint_lua_colon_call_self
  sink(self.data)
end
M:leak()
