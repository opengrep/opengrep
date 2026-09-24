def a(x)
  # ERROR:
  unless x.ok?
    bail
  end
  # ERROR:
  if !x.ok?
    bail
  end
  # ERROR:
  unless x.ok?
    bail
  else
    other
  end
  unless x.ok?
    other
  else
    bail
  end
end
