require_relative 'InternalSource'
require_relative 'test_false_negative'
require_relative 'test_false_positive'
require_relative 'zz_main'
class IgnoresArg
  def initialize(data)
    @data = "safe"
  end

  def get_data
    @data
  end
end
