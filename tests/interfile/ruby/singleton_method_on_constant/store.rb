class Store
  def self.build(data)
    # ruleid: singleton-method-on-constant
    sink(data)
  end

  def build(data)
    # ok: singleton-method-on-constant
    sink(data)
  end
end
