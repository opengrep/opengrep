def run
  # ERROR:
  foo(1, 2)

  # ERROR:
  foo(a_very_long_constant_name, 2)

  # ERROR:
  foo(unsafe, 2)

  foo(2, 1)
end
