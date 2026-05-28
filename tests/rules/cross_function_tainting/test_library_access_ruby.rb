# Field-sensitive taint through Ruby Hash#fetch / Object#send /
# Object#public_send / Hash#dig library-call idioms.

def handler_fetch_pos(h)
  # ruleid: test-library-access-taint
  sink(h.fetch(:body))
end

def caller_fetch_pos
  handler_fetch_pos({ body: source(), user: "safe" })
end

def handler_fetch_neg(h)
  # ok: test-library-access-taint
  sink(h.fetch(:body))
end

def caller_fetch_neg
  handler_fetch_neg({ body: "safe", user: source() })
end

# fetch(:k, default): the default is evaluated eagerly; its taint
# flows into the result via the conditional `if tmp == nil then tmp
# = default` branch.
def handler_fetch_default(h)
  # ruleid: test-library-access-taint
  sink(h.fetch(:body, source()))
end

def caller_fetch_default
  handler_fetch_default({ body: "safe" })
end

# send(:method): reflective single-key read; same field-sensitivity
# applies.
def handler_send_pos(obj)
  # ruleid: test-library-access-taint
  sink(obj.send(:body))
end

def caller_send_pos
  handler_send_pos({ body: source(), user: "safe" })
end

# dig(:a, :b): chained literal keys → precise nested Fetch.
def handler_dig_pos(h)
  # ruleid: test-library-access-taint
  sink(h.dig(:a, :b))
end

def caller_dig_pos
  handler_dig_pos({ a: { b: source(), c: "safe" } })
end

def handler_dig_neg(h)
  # ok: test-library-access-taint
  sink(h.dig(:a, :b))
end

def caller_dig_neg
  handler_dig_neg({ a: { b: "safe", c: source() } })
end
