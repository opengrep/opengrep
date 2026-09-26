module Loud
  def handle(x)
    # ruleid: include-arguments-order
    sink(x)
  end
end

module Quiet
  def handle(x)
    # ok: include-arguments-order
    sink(x)
  end
end

class C
  include Loud, Quiet
end

def run
  C.new.handle(source())
end
