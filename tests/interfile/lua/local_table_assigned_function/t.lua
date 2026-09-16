local M = {}

M.f = function(x)
  -- ruleid: local_table_assigned_function
  sink(x)
end

function g()
  M.f(source())
end

g()
