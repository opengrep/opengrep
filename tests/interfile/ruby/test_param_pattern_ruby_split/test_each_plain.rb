require_relative 'test_each_destructure'
require_relative 'test_lambda_destructure'
require_relative 'zz_main'
def test_each_plain
  xs = [source()]
  xs.each { |a|
    # ruleid: test-param-pattern-taint
    sink(a)
  }
end
