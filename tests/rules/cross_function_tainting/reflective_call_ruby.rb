# The methods __send__ and method look a method up by a symbol on the
# receiver's class at run time, so they reach that class's method only.
class Handler
  def m(x)
    # ruleid: reflective_call_ruby
    sink(x)
  end

  def n(x)
    # ruleid: reflective_call_ruby
    sink(x)
  end
end

class Other
  def m(x)
    # ok: reflective_call_ruby
    sink(x)
  end

  def n(x)
    # ok: reflective_call_ruby
    sink(x)
  end
end

def run_callback(cb)
  cb.call(source())
end

def main
  h = Handler.new
  h.__send__(:m, source())
  run_callback(h.method(:n))
end
