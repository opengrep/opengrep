def go
  Store.new.save(source())
end

def go_wrap
  Store.new.wrap(source())
end
