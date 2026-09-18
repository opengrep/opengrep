M = {}

function M.f(x)
  -- ruleid: global_table_same_file
  sink(x)
end

function g()
  M.f(source())
end

g()
