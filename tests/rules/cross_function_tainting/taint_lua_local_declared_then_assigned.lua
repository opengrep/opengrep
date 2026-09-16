local f

f = function(x)
  -- ruleid: taint_lua_local_declared_then_assigned
  sink(x)
end

f(source())
