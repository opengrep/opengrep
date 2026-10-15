-- A global variable, which naming does not resolve, read by a local function
-- written before the assignment that creates it: the function captures it.
function f()
  local function inner()
    -- ruleid: lua_lambda_reads_unresolved_global_before_write
    sink(z)
  end
  z = source()
  inner()
end

-- A local declared after the function is a different variable.
function g()
  local function inner()
    -- ok: lua_lambda_reads_unresolved_global_before_write
    sink(w)
  end
  local w = source()
  inner()
end
