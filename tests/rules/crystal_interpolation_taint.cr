def a(safe)
  x = "SELECT #{source()}"
  # ruleid: crystal_interpolation_taint
  sink(x)

  y = "SELECT #{safe}"
  # ok: crystal_interpolation_taint
  sink(y)

  z = <<-SQL
    SELECT #{source()}
    SQL
  # ruleid: crystal_interpolation_taint
  sink(z)
end
