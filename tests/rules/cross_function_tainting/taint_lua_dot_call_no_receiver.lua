local M = {}
M.data = source()
function M:leak()
  -- ok: taint_lua_dot_call_no_receiver
  sink(self.data)
end
M.leak()
