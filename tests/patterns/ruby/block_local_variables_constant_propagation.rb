def f
  y = 4
  # OK:
  bar { |x; y| g(y) }
  # ERROR:
  bar { |x| g(y) }
end
