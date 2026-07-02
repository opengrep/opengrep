require_relative 'handler_fetch_pos'
require_relative 'caller_fetch_pos'
require_relative 'handler_fetch_neg'
require_relative 'caller_fetch_neg'
require_relative 'handler_fetch_default'
require_relative 'caller_fetch_default'
require_relative 'handler_send_pos'
require_relative 'caller_send_pos'
require_relative 'handler_dig_pos'
require_relative 'handler_dig_neg'
require_relative 'caller_dig_neg'
require_relative 'zz_main'
def caller_dig_pos
  handler_dig_pos({ a: { b: source(), c: "safe" } })
end
