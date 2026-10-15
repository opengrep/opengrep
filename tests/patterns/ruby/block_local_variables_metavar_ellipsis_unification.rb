# Each block declares its own block-local variables: two blocks share none.
def different_variables
  bar { |x; y, z| foo(x) }
  baz { |x; y, z| foo(x) }
end

def none
  # ERROR:
  bar { |x| foo(x) }
  baz { |x| foo(x) }
end
