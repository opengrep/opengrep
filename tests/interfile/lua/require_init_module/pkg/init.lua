local M = {}

function M.leak(v)
  -- ruleid: require_init_module
  sink(v)
end

return M
