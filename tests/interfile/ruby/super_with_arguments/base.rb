class Base
  def handle(data)
    # ok: super-with-arguments
    sink(data)
  end

  def audit(data)
    # ruleid: super-with-arguments
    sink(data)
  end
end
