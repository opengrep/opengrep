def foo() do
  x = source()
  cond do
    # ruleid: cond_taint_rule
    y = sink(x) -> 3
  end
end
