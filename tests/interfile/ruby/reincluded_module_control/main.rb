module M
  def handle(x)
    # ok: reincluded-module-control
    sink(x)
  end
end

class A
  include M

  def handle(x)
    # ruleid: reincluded-module-control
    sink(x)
  end
end

class B < A
end

def run
  B.new.handle(source())
end
