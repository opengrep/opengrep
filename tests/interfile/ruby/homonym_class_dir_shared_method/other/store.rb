# The constant path of this class is Other::Store, so neither of its methods
# is reachable through a reference to Store.
module Other
  class Store
    def save(data)
      # ok: homonym-class-dir-shared-method
      sink(data)
    end

    def wrap(data)
      # ok: homonym-class-dir-shared-method
      sink(data)
    end
  end
end
