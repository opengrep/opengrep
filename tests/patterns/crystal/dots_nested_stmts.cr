def run(value)
  # ERROR:
  if value != 3
    foo
  end

  # ERROR:
  if value == 1
    foo
    bar
    baz
  end
end
