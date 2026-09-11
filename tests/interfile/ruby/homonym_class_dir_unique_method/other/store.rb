class Store
  def save(data)
    # ok: homonym-class-dir-unique-method
    sink(data)
  end

  def wrap(data)
    # ruleid: homonym-class-dir-unique-method
    sink(data)
  end
end
