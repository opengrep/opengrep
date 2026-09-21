def last_expression(x)
  # ruleid: ruby_implicit_return_yield
  yield x
end

def not_last(x)
  # ok: ruby_implicit_return_yield
  yield x
  nil
end
