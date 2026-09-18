def go
  Notes.record(source())
  Audit.write(source())
  Audit.ignore(source())
end
