def a(t)
  # ERROR:
  foo(<<~A, <<~B)
    one
  A
    two #{t}
  B
  foo(<<~A, <<~B)
    one #{t}
  A
    two
  B
end
