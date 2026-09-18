class B
  def self.leak(v)
    # ok: class-path-identity
    sink(v)
  end
end
