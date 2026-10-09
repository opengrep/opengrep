class Mapping
  def read_safe
    # ok: ruby_ctor_checked_first
    sink(@safe)
  end

  def initialize(x)
    @path = source(x)
    @safe = "constant"
  end
end

class Other
  def read_path
    # ruleid: ruby_ctor_checked_first
    sink(path)
  end

  attr_reader :path

  def initialize(x)
    @path = source(x)
    read_path()
  end
end
