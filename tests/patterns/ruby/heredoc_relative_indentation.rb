def a(t)
  # ERROR:
  run(<<~SQL)
    SELECT *
      FROM users
  SQL
  # ERROR:
  run(<<~SQL)
        SELECT *

      FROM users
    WHERE 1
  SQL
  run(<<~SQL)
    SELECT *
    FROM users
  SQL
end
