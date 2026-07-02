require_relative 'handler_fetch_pos'
require_relative 'caller_fetch_pos'
require_relative 'handler_fetch_neg'
require_relative 'caller_fetch_neg'
require_relative 'handler_fetch_default'
require_relative 'caller_fetch_default'
require_relative 'handler_send_pos'
require_relative 'caller_send_pos'
require_relative 'handler_dig_pos'
require_relative 'caller_dig_pos'
require_relative 'handler_dig_neg'
require_relative 'caller_dig_neg'
# Field-sensitive taint through Ruby Hash#fetch / Object#send /
# Object#public_send / Hash#dig library-call idioms.





# fetch(:k, default): the default is evaluated eagerly; its taint
# flows into the result via the conditional `if tmp == nil then tmp
# = default` branch.


# send(:method): reflective single-key read; same field-sensitivity
# applies.


# dig(:a, :b): chained literal keys → precise nested Fetch.



