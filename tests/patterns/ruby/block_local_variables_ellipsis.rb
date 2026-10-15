# ERROR:
bar { |x| foo(x) }

# ERROR:
bar { |x; y| foo(x) }

# ERROR:
bar { |x; y, z| foo(x) }

# OK:
bar { |z; y| foo(z) }
