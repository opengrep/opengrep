# Ruby: nested lambdas invoked via .call()
def test_call()
  x = source()
  outer = ->(a) {
    inner = ->(b) {
      # ruleid: test-invoke-methods-ruby
      sink(b)
    }
    inner.call(a)
  }
  outer.call(x)
end

# Negative: no taint
def test_no_taint()
  x = "clean"
  outer = ->() {
    inner = ->() {
      # ok: test-invoke-methods-ruby
      sink(x)
    }
    inner.call()
  }
  outer.call()
end

def source()
  "tainted"
end

def sink(x)
end
