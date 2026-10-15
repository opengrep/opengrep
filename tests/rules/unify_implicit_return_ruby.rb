# An expression returned as a method's last one is still the same
# expression as an equal one elsewhere.
def f
  # ruleid: unify_implicit_return_ruby
  g(bar())
  bar()
end

def h
  # ruleid: unify_implicit_return_ruby
  g(bar())
  bar()
  baz()
end

def k
  # ok: unify_implicit_return_ruby
  g(bar())
  qux()
end
