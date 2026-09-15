class A::B
  def self.run(v)
    leak(v)
  end
end
