class Base
  def handle(data)
    # ok: super-calls-parent-method
    sink(data)
  end

  def audit(data)
    # ruleid: super-calls-parent-method
    sink(data)
  end
end
