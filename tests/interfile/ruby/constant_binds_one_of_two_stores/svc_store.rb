module Svc
  class Store
    def save(data)
      # ruleid: constant-binds-one-of-two-stores
      sink(data)
    end
  end
end
