# ERROR:
bar { |x; y| foo(x) }

# OK:
bar { |x; z, y| foo(x) }

# OK:
bar { |x| foo(x) }

# ERROR:
bar do |x; y|
  foo(x)
end
