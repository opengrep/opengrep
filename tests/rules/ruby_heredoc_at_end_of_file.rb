def a(t)
  # ruleid: ruby_heredoc_at_end_of_file
  run(<<~SQL)
    DELETE FROM users
  SQL