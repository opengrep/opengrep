class Grand
  def handle(data)
    # ruleid: member-after-unresolved-module-grandchild
    sink(data)
  end
end

class Base < Grand
  include Enumerable
end

class ShadowedGrand
  def handle(data)
    # ok: member-after-unresolved-module-grandchild
    sink(data)
  end
end

class OwnBase < ShadowedGrand
  include Enumerable

  def handle(data)
    data
  end
end
