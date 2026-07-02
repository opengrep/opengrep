require_relative 'test_call'
require_relative 'source'
require_relative 'sink'
require_relative 'zz_main'
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
