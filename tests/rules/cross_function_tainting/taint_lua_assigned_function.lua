f = function(x)
  -- ruleid: taint_lua_assigned_function
  sink(x)
end

f(source())
