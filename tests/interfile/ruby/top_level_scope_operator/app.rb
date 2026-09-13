module Svc
  class Runner
    def run
      ::Store.new.save(source())
    end
  end
end
