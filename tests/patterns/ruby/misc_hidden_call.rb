#ERROR: match
foo(1,2,3)

# A bare identifier with no prior assignment is a method call in Ruby.
# Disambiguate_ruby_calls wraps it in Call() after naming.
#ERROR: match
foo

a = bar
#ERROR: match
b = foo

# A bare identifier WITH a prior assignment is a local variable,
# not a method call. It should NOT match foo(...).
foo = 42
c = foo

# A parameter is also not a method call.
def use_param(foo)
  d = foo
end
