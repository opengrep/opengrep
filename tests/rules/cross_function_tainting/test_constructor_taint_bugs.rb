# FALSE NEGATIVE: zero-arg constructor with internal source.
# The taint engine never analyzes initialize, so @data is not tainted.
class InternalSource
  def initialize
    @data = source()
  end

  def get_data
    @data
  end
end

def test_false_negative
  obj = InternalSource.new()
  result = obj.get_data()
  # ruleid: constructor-taint-bugs
  sink(result)
end

# FALSE POSITIVE: constructor ignores its argument.
# The taint engine leaks source() through all_args_taints
# even though initialize never stores it.
class IgnoresArg
  def initialize(data)
    @data = "safe"
  end

  def get_data
    @data
  end
end

def test_false_positive
  obj = IgnoresArg.new(source())
  result = obj.get_data()
  # ok: constructor-taint-bugs
  sink(result)
end
