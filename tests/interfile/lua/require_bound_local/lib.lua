local M = {}

function M.leak(v)
  -- ruleid: require_bound_local
  sink(v)
end

return M
