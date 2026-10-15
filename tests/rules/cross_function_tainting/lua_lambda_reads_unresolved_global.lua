-- A global variable created by an assignment, which naming does not resolve,
-- read by a function written in a function: that function captures it.
function assigns_a_global()
  y = source()
  local g = function()
    -- ruleid: lua_lambda_reads_unresolved_global
    sink(y)
  end
  g()
end

function assigns_an_untainted_global()
  z = clean()
  local g = function()
    -- ok: lua_lambda_reads_unresolved_global
    sink(z)
  end
  g()
end
