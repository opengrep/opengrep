require_relative 'test_no_taint'
require_relative 'source'
require_relative 'sink'
require_relative 'zz_main'
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
