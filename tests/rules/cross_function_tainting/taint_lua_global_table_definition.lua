M = {}
function M.f(x)
  -- ruleid: taint_lua_global_table_definition
  sink(x)
end
function g()
  M.f(source())
end
g()
