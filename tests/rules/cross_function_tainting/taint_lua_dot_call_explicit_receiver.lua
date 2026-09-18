local M = {}
M.data = source()
function M:leak()
  -- ruleid: taint_lua_dot_call_explicit_receiver
  sink(self.data)
end
M.leak(M)
