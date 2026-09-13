class Store
  def save(data)
    # ok: assigned-receiver-keeps-qualifier
    sink(data)
  end
end
