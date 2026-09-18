function f(x)
  -- ruleid: taint_lua_function_statement
  sink(x)
end

f(source())
