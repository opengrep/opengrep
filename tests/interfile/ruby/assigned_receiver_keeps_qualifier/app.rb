def go
  store = Svc::Store.new
  store.save(source())
end
