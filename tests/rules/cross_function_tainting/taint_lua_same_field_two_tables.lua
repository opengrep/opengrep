local A = {}
local B = {}
function A.f(x)
  -- ok: taint_lua_same_field_two_tables
  sink(x)
end
function B.f(x)
  -- ruleid: taint_lua_same_field_two_tables
  sink(x)
end
function g()
  B.f(source())
end
g()
