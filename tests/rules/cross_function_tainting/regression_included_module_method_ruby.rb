# A class definition that reopens a class adds to the same class, so the
# modules included by each of its definitions are all its ancestors.
module FromA
  def from_a(x)
    # ruleid: regression_included_module_method_ruby
    sink(x)
  end
end

module NotIncluded
  def from_a(x)
    # ok: regression_included_module_method_ruby
    sink(x)
  end
end

class Foo
  include FromA
end

Foo.new.from_a(source())
