def a(t)
  # ERROR:
  x = <<-SQL
    SELECT * FROM #{t}
    SQL
  w = <<-SQL
    SELECT 1
    SQL
  # ERROR:
  y = "SELECT * FROM #{t}"
end
