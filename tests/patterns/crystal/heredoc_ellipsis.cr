def a(x)
  # ERROR:
  foo("plain")
  foo("interp #{x}")
  # ERROR:
  foo(<<-A)
    plain heredoc
    A
  foo(<<-A)
    interp #{x}
    A
end
