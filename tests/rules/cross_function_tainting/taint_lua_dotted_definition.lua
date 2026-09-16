local M = {}
function M.f(x)
  -- ruleid: taint-lua-dotted-definition
  sink(x)
end
M.f(source())
