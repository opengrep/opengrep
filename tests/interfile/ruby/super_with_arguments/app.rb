def go
  Child.new.handle(source())
  Child.new.audit(source())
end
