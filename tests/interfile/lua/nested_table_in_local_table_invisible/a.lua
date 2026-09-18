local M = {}
M.a = {}

function M.a.f(x)
  -- ok: nested_table_in_local_table_invisible
  sink(x)
end
