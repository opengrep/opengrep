def a(cols, rest)
  # ruleid: ruby_heredoc_dots_in_target
  foo(<<-A)
  ...
  A

  # ok: ruby_heredoc_dots_in_target
  foo(<<-A)
  anything else
  A

  # ok: ruby_heredoc_dots_in_target
  foo("...")
end
