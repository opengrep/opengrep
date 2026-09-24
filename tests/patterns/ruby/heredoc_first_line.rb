def a
  # ERROR:
  run(<<~SQL)
    DROP TABLE users
  SQL
  # ERROR:
  run(<<SQL)
DROP TABLE users
SQL
  run(<<-SQL)
    DROP TABLE users
  SQL
end
