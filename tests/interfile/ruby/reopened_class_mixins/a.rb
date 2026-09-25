# A class definition that reopens a class adds to the same class, so the
# modules included by each of its definitions are all its ancestors.
module FromA
  def from_a(x)
    # ruleid: reopened_class_mixins
    sink(x)
  end
end

module NotIncluded
  def from_a(x)
    # ok: reopened_class_mixins
    sink(x)
  end
end

class Foo
  include FromA
end
