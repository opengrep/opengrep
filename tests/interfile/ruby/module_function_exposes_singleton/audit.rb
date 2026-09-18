module Audit
  module_function :write

  def write(data)
    # ruleid: module-function-exposes-singleton
    sink(data)
  end

  def ignore(data)
    # ok: module-function-exposes-singleton
    sink(data)
  end
end
