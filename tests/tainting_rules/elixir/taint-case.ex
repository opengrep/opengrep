def foo() do
  a = source()

  b = case a do
    0 -> a
    x -> x
  end

  # ruleid: case_taint_rule
  sink(b)
end
