require_relative 'InternalSource'
require_relative 'IgnoresArg'
require_relative 'test_false_positive'
require_relative 'zz_main'
def test_false_negative
  obj = InternalSource.new()
  result = obj.get_data()
  # ruleid: constructor-taint-bugs
  sink(result)
end
