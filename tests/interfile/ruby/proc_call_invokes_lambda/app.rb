def go
  callback = Proc.new { |value| store(value) }
  callback.call(source())
end
