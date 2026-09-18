local M = {}

function M.f(x)
  -- ok: local_table_invisible
  sink(x)
end
