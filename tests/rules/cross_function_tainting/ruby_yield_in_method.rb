# Methods that yield to an implicit block, in a class: the file is still
# analysed with intrafile taint, and taint passes through the yield into the
# block.
class Runner
  def run
    yield
    # ruleid: ruby-yield-in-method
    sink(source())
  end

  def each_value(x)
    yield x
  end
end

def tainted_value
  # ruleid: ruby-yield-in-method
  Runner.new.each_value(source()) { |v| sink(v) }
end

def safe_value
  # ok: ruby-yield-in-method
  Runner.new.each_value("safe") { |v| sink(v) }
end
