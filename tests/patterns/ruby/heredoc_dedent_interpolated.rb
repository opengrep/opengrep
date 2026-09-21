def a(t)
  # ERROR:
  run(<<~SQL)
        SELECT *
          FROM #{t}
  SQL
  run(<<~SQL)
    SELECT *
    FROM #{t}
  SQL
end
