local M = {}
M.a = {}

function M.a.f(x)
  -- ruleid: nested_table_assigned
  sink(x)
end

function g()
  M.a.f(source())
end

g()
