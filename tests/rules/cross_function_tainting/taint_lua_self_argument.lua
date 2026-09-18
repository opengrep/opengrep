local M = {}
local function report(x)
  -- ruleid: taint_lua_self_argument
  sink(x)
end
M.data = source()
function M:leak()
  report(self.data)
end
M:leak()
