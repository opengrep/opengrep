def run
  # ERROR:
  foo
  bar

  # ERROR:
  foo
  value = bar

  # ERROR:
  foo
  puts bar

  # ERROR:
  foo
  return bar
end
