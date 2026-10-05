# A method that yields to an implicit block, in a class: the file is still
# analysed with intrafile taint.
class Runner
  def run
    yield
    # ruleid: ruby-yield-in-method
    sink(source())
  end
end
