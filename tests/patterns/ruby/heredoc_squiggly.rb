def a(t)
  # ERROR:
  run(<<~SQL)
    DROP TABLE users
  SQL
  # ERROR:
  run(<<~SQL)
      SELECT 1;

    DROP TABLE users
  SQL
  run(<<-SQL)
    DROP TABLE users
  SQL
end
