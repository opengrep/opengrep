def a(x)
  foo("plain")
  # ERROR:
  foo("interp #{x}")
  foo(<<-A)
    plain heredoc
    A
  # ERROR:
  foo(<<-A)
    interp #{x}
    A
end
