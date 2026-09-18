class C
  def self.leak(v)
    # ruleid: class-method
    sink(v)
  end

  def leak(v)
    # ok: class-method
    sink(v)
  end
end
