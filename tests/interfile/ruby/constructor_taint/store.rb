class Store
  def initialize(data)
    @data = data
  end

  def send
    # ruleid: test-constructor-taint
    sink(@data)
  end
end

def sink(x)
  puts x
end
