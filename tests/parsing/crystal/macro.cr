macro define_getter(name)
  def {{name.id}}
    @{{name.id}}
  end
end

class Box
  define_getter value

  def initialize(@value : Int32)
  end
end
