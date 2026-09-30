class Base
  def handle(data)
    # ruleid: member-after-unresolved-module
    sink(data)
  end
end

module Helpers
  def audit(data)
    # ruleid: member-after-unresolved-module
    sink(data)
  end
end

class ShadowedBase
  def handle(data)
    # ok: member-after-unresolved-module
    sink(data)
  end
end
