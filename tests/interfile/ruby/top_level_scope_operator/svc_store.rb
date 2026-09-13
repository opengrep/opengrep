module Svc
  class Store
    def save(data)
      # ok: top-level-scope-operator
      sink(data)
    end
  end
end
