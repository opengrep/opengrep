def guard_sink(x) do
  dangerous()
catch
  # ERROR:
  _kind, _value when test() -> :ok
end
