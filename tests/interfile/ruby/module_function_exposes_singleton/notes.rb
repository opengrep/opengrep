module Notes
  extend self

  def record(data)
    # ruleid: module-function-exposes-singleton
    sink(data)
  end
end
