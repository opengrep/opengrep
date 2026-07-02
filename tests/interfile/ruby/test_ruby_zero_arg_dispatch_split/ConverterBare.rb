require_relative 'Converter'
require_relative 'test_new_with_parens'
require_relative 'test_new_no_parens'
require_relative 'test_new_with_parens_get_no_parens'
require_relative 'test_new_no_parens_get_no_parens'
require_relative 'test_bare_source_as_arg'
require_relative 'test_bare_source_not_variable'
require_relative 'test_bare_source_is_variable'
require_relative 'TaintedService'
require_relative 'test_zero_arg_new_with_parens'
require_relative 'test_zero_arg_new_no_parens'
require_relative 'zz_main'
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
