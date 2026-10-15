# ERROR:
bar { |x; y| foo(x) }

# ERROR:
bar { |x; z, y| foo(x) }

# OK:
bar { |x; z| foo(x) }
