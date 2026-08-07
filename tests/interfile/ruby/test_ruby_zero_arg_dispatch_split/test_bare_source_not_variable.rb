require_relative 'Converter'
require_relative 'test_new_with_parens'
require_relative 'test_new_no_parens'
require_relative 'test_new_with_parens_get_no_parens'
require_relative 'test_new_no_parens_get_no_parens'
require_relative 'ConverterBare'
require_relative 'test_bare_source_as_arg'
require_relative 'test_bare_source_is_variable'
require_relative 'TaintedService'
require_relative 'test_zero_arg_new_with_parens'
require_relative 'test_zero_arg_new_no_parens'
require_relative 'zz_main'
def test_bare_source_not_variable
  x = source
  # ruleid: test-ruby-zero-arg-dispatch
  sink(x)
end
