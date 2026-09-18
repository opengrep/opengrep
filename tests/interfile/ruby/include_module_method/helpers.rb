module Helpers
  # Mixed into App via `include Helpers`.  The sink lives here; the
  # cross-file dispatch is only reachable when projidx records the
  # `include` as a parent edge from App to Helpers.
  def process(data)
    # ruleid: include-module-method
    sink(data)
  end
end

def sink(x)
  puts x
end
