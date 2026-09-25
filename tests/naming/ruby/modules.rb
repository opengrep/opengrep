module FromA
  LIMIT = 1

  def from_a(x)
    x + LIMIT
  end
end

module NotIncluded
  def from_a(x)
    x
  end
end
