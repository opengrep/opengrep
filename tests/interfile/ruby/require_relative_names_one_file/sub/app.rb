require_relative 'store'

def go
  store = Store.new
  store.save(source())
  store.wrap(source())
end
