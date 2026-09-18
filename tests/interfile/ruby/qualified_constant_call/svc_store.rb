module Svc
  class Store
    def self.save(data)
      # ruleid: qualified-constant-call
      sink(data)
    end

    def save(data)
      # ok: qualified-constant-call
      sink(data)
    end
  end
end
