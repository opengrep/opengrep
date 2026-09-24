def a(cols, rest)
  # ruleid: crystal_heredoc_dots_in_target
  foo(<<-A)
    ...
  A

  # ok: crystal_heredoc_dots_in_target
  foo(<<-A)
    anything else
  A

  # ok: crystal_heredoc_dots_in_target
  foo("...")
end
