# Test that hash/array access `obj[:key]` propagates taint correctly.
# `obj[:key]` was misparsed as Call(DotAccess(obj, Op_AREF), [:key])
# instead of ArrayAccess, which broke taint propagation because
# a Call to `[]` has no signature, while ArrayAccess preserves taint.

class TestController
  def show
    if continue_params[:to]
      # ruleid: test-ruby-array-access
      sink(continue_params[:to])
    end
  end

  def continue_params
    source()
  end
end

def source()
  "tainted"
end

def sink(x)
end
