function g()
  f(source())
end

f = function(x)
  -- ruleid: taint_lua_forward_reference
  sink(x)
end

g()
