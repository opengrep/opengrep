class Converter
  def initialize(data)
    @data = data
  end

  def taint_method
    return source()
  end

  def get_data
    @data
  end

  def use_method_with_parens
    # ruleid: test-ruby-zero-arg-dispatch
    sink(self.taint_method())
  end

  def use_method_no_parens
    # ruleid: test-ruby-zero-arg-dispatch
    sink(self.taint_method)
  end
end

def test_new_with_parens
  obj = Converter.new(source())
  result = obj.get_data()
  # ruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

def test_new_no_parens
  obj = Converter.new source()
  result = obj.get_data()
  # ruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

def test_new_with_parens_get_no_parens
  obj = Converter.new(source())
  result = obj.get_data
  # ruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

def test_new_no_parens_get_no_parens
  obj = Converter.new source()
  result = obj.get_data
  # ruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

# Bare identifier disambiguation: `source` (no parens) is a method call
# when no prior assignment exists, and should be matched by `source(...)`.

class ConverterBare
  def initialize(data)
    @data = data
  end

  def taint_method
    return source
  end

  def get_data
    @data
  end

  def use_bare_source_method
    # ruleid: test-ruby-zero-arg-dispatch
    sink(self.taint_method())
  end
end

def test_bare_source_as_arg
  obj = Converter.new(source)
  result = obj.get_data()
  # ruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

def test_bare_source_not_variable
  x = source
  # ruleid: test-ruby-zero-arg-dispatch
  sink(x)
end

def test_bare_source_is_variable
  source = "safe"
  x = source
  # ok: test-ruby-zero-arg-dispatch
  sink(x)
end

class TaintedService
  def initialize
    @data = source
  end

  def get_data
    @data
  end
end

def test_zero_arg_new_with_parens
  obj = TaintedService.new()
  result = obj.get_data()
  # todoruleid: test-ruby-zero-arg-dispatch
  sink(result)
end

def test_zero_arg_new_no_parens
  obj = TaintedService.new
  result = obj.get_data()
  # todoruleid: test-ruby-zero-arg-dispatch
  sink(result)
end
