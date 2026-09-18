local M = {}

function M.leak(v)
  -- ruleid: require_of_multiple_returns
  sink(v)
end

return M, 1
