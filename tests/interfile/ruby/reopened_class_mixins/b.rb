require_relative "a"

module FromB
  def from_b(x)
    from_a(x)
  end
end

class Foo
  include FromB

  def run(x)
    from_b(x)
  end
end

Foo.new.run(source())
