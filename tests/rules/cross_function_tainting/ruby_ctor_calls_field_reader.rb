class Mapping
  def initialize(x)
    @path = source(x)
    @safe = "constant"
    @read = read_path
    @kept = read_safe
  end

  attr_reader :path, :safe

  def read_path
    # ruleid: ruby_ctor_calls_field_reader
    sink(path)
  end

  def read_safe
    # ok: ruby_ctor_calls_field_reader
    sink(safe)
  end
end
