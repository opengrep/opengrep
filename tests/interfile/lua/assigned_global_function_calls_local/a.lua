local function leak(y)
  -- ruleid: assigned_global_function_calls_local
  sink(y)
end

f = function(x)
  leak(x)
end
