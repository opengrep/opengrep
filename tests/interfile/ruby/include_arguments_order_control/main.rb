module Loud
  def handle(x)
    # ruleid: include-arguments-order-control
    sink(x)
  end
end

module Quiet
  def handle(x)
    # ok: include-arguments-order-control
    sink(x)
  end
end

class C
  include Quiet
  include Loud
end

def run
  C.new.handle(source())
end
