module M
  def handle(x)
    # ok: reincluded-module
    sink(x)
  end
end

class A
  include M

  def handle(x)
    # ruleid: reincluded-module
    sink(x)
  end
end

class B < A
  include M
end

def run
  B.new.handle(source())
end
