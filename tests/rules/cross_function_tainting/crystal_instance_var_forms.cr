class Shorthand
  def initialize(@path : String)
  end

  def read
    # ruleid: crystal_instance_var_forms
    sink(@path)
  end
end

class Typed
  def set
    @path : String = source()
    read
  end

  def read
    # ruleid: crystal_instance_var_forms
    sink(@path)
  end
end

class ExplicitSelf
  def set
    @path = source()
    read
  end

  def read
    # ruleid: crystal_instance_var_forms
    sink(self.@path)
  end
end

class LocalNotField
  def set
    path = source()
    @path = "constant"
    read
  end

  def read
    # ok: crystal_instance_var_forms
    sink(@path)
  end
end

def caller_of_shorthand
  Shorthand.new(source()).read
end
