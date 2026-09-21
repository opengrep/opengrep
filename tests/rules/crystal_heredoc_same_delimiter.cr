def a(t)
  # ruleid: crystal_heredoc_same_delimiter
  foo(<<-A, <<-A)
    one
    A
    two #{t}
    A

  # ok: crystal_heredoc_same_delimiter
  foo(<<-A, <<-A)
    one #{t}
    A
    two
    A

  # ruleid: crystal_heredoc_same_delimiter
  foo(<<-A, <<-B)
    one
    A
    two #{t}
    B

  x = <<-A
    one #{t}
    A
  # ruleid: crystal_heredoc_same_delimiter
  foo(<<-A, "two #{t}")
    one
    A
end
