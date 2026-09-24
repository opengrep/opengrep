def a(t)
  # ERROR:
  x = <<-SQL
    SELECT * FROM #{t}
  SQL
  # ERROR:
  y = <<~SQL
    SELECT * FROM #{t}
  SQL
  # ERROR:
  z = <<SQL
SELECT * FROM #{t}
SQL
  w = <<~SQL
    SELECT 1
  SQL
  v = <<~'SQL'
    SELECT * FROM #{t}
  SQL
end
