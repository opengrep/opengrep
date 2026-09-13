module Svc
  class Runner
    def run
      store = Store.new(source())
      Audit.new.record(store.data)
    end
  end
end
