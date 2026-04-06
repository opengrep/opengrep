def foo() do
  # ERROR:
  [1,2,3]

  # OK:
  [1,2|3]
end
