# Field-sensitive taint through an Elixir map destructure.
# The destructure binds `body` to the `:body` key and `user` to
# `:user`. At the caller, we pass maps where exactly one key carries
# a source; only the sink whose destructured leaf matches the
# tainted key should fire.

def handler_pos(%{body: body, user: user}) do
  # ruleid: test-map-destructure-taint
  sink(body)
end

def caller_pos do
  handler_pos(%{body: source(), user: "safe"})
end

def handler_neg(%{body: body, user: user}) do
  # ok: test-map-destructure-taint
  sink(body)
end

def caller_neg do
  handler_neg(%{body: "safe", user: source()})
end
