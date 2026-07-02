require_relative 'test_lambda_destructure'
require_relative 'test_each_plain'
require_relative 'zz_main'
def test_each_destructure
  pairs = [[source(), "y"]]
  pairs.each { |(a, _)|
    # ruleid: test-param-pattern-taint
    sink(a)
  }
end
