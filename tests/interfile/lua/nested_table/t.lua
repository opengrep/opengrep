local M = { a = {} }

function M.a.f(x)
  -- ruleid: nested_table
  sink(x)
end

function g()
  M.a.f(source())
end

g()
