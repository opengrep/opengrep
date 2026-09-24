def a(t)
  # ERROR:
  run(<<~SQL)
    DELETE FROM users
  SQL
  run(<<~SQL)
    SELECT 1
  SQL
  # ERROR:
  run(<<~'SQL')
    DELETE FROM users
  SQL
  # ERROR:
  run(<<-SQL)
    DELETE FROM users
  SQL
  run("SELECT 1")
end
