# A target of a tuple assignment, which naming does not resolve, read by a
# lambda: the lambda captures it.
function tuple_assignment()
  a, b = source(), 1
  # ruleid: julia_lambda_reads_unresolved_variable
  g = () -> sink(a)
  g()
end

# The same with [local y = e].
function local_declaration()
  local y = source()
  # ruleid: julia_lambda_reads_unresolved_variable
  g = () -> sink(y)
  g()
end

function clean()
  a, b = 1, source()
  # ok: julia_lambda_reads_unresolved_variable
  g = () -> sink(a)
  g()
end
