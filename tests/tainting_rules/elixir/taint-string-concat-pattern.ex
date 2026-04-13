def foo("a" <> x) do
  #ruleid: taint
  sink(x)
end
