def a(t)
  # ruleid: ruby_heredoc_same_delimiter
  foo(<<~A, <<~A)
    one
  A
    two #{t}
  A

  # ok: ruby_heredoc_same_delimiter
  foo(<<~A, <<~A)
    one #{t}
  A
    two
  A

  # ruleid: ruby_heredoc_same_delimiter
  foo(<<~A, <<~B)
    one
  A
    two #{t}
  B

  x = <<~A
    one #{t}
  A
  # ruleid: ruby_heredoc_same_delimiter
  foo(<<~A, "two #{t}")
    one
  A
end
