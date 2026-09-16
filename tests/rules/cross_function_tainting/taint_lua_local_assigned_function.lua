local f = function(x)
  -- ruleid: taint_lua_local_assigned_function
  sink(x)
end

f(source())
