require_relative 'custom_map'
require_relative 'custom_for_each'
require_relative 'test_custom_map'
require_relative 'test_custom_foreach'
require_relative 'test_builtin_each'
require_relative 'test_builtin_select'
require_relative 'source'
require_relative 'sink'
require_relative 'toplevel_handler'
require_relative 'zz_main'
def test_builtin_map
  arr = [source()]
  arr.map do |x|
    # ruleid: test-hof-taint
    sink(x)
  end
end
