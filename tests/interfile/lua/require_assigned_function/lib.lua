local M = {}

M.leak = function(v)
  -- ruleid: require_assigned_function
  sink(v)
end

return M
