local M = { a = {} }
function M.a.f(x)
  -- ruleid: taint_lua_nested_table_definition
  sink(x)
end
function g()
  M.a.f(source())
end
g()
