require_relative 'test_false_negative'
require_relative 'IgnoresArg'
require_relative 'test_false_positive'
require_relative 'zz_main'
class InternalSource
  def initialize
    @data = source()
  end

  def get_data
    @data
  end
end
