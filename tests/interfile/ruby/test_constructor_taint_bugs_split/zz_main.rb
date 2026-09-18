require_relative 'InternalSource'
require_relative 'test_false_negative'
require_relative 'IgnoresArg'
require_relative 'test_false_positive'
# FALSE NEGATIVE: zero-arg constructor with internal source.
# The taint engine never analyzes initialize, so @data is not tainted.


# FALSE POSITIVE: constructor ignores its argument.
# The taint engine leaks source() through all_args_taints
# even though initialize never stores it.

