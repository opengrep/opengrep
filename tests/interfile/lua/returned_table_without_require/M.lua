local M = {}

function M.f(x)
  -- ok: returned_table_without_require
  sink(x)
end

return M
