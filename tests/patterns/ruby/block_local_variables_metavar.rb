# ERROR:
bar { |x; y| foo(x) }

# OK:
bar { |x| foo(x) }

# OK:
bar { |x; y, z| foo(x) }
