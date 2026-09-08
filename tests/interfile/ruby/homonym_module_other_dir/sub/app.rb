require_relative 'store'

def go
  Store.new.save(source())
end
