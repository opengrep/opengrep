def a(t)
  # ERROR:
  foo(<<-A, <<-B)
    one
    A
    two DELETE
    B
  foo(<<-A, <<-B)
    one DELETE
    A
    two
    B
end
