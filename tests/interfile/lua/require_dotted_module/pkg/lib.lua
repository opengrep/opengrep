local M = {}

function M.leak(v)
  -- ruleid: require_dotted_module
  sink(v)
end

return M
