function g(y)
  f = function(x)
    -- ruleid: taint_lua_assignment_in_function_body
    sink(x)
  end
  f(y)
end

g(source())
