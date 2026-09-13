module Svc
  class Store
    def save(data)
      # ruleid: assigned-receiver-keeps-qualifier
      sink(data)
    end
  end
end
