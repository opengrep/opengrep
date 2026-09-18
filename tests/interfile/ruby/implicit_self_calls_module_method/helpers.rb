module Helpers
  def store(data)
    # ruleid: implicit-self-calls-module-method
    sink(data)
  end
end
