def go
  Store.new.send(:save, source())
end
