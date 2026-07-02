require_relative 'test_each_destructure'
require_relative 'test_each_plain'
require_relative 'zz_main'
def test_lambda_destructure
  lam = ->((a, _b)) {
    # ruleid: test-param-pattern-taint
    sink(a)
  }
  lam.call([source(), "y"])
end
