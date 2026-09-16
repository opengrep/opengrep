local function f(x, n)
  if n == 0 then
    -- ruleid: taint_lua_local_function_recursion
    sink(x)
  else
    f(x, n - 1)
  end
end

f(source(), 2)
