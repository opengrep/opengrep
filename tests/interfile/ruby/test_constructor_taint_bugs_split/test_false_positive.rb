require_relative 'InternalSource'
require_relative 'test_false_negative'
require_relative 'IgnoresArg'
require_relative 'zz_main'
def test_false_positive
  obj = IgnoresArg.new(source())
  result = obj.get_data()
  # ok: constructor-taint-bugs
  sink(result)
end
