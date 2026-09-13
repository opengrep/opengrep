module Other
  class Store
    def save(data)
      # ok: constant-binds-one-of-two-stores
      sink(data)
    end
  end
end
