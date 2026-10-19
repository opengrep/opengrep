# ERROR:
bar { |x; y| foo(x) }

# ERROR:
bar { |x; y, z| foo(x) }

# ERROR:
bar { |x; z, y| foo(x) }

# OK:
bar { |x; z| foo(x) }

# OK:
bar { |x| foo(x) }
