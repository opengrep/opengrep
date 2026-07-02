require_relative 'Controller'
require_relative 'source'
require_relative 'sink'
# Test that taint flows through chained method calls where the receiver
# is itself a method call: get_data.strip should call get_data() first.



