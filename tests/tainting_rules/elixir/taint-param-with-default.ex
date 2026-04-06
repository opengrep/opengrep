def foo(x \\ "default value") do
  # ruleid: taint
  sink(x)
end
