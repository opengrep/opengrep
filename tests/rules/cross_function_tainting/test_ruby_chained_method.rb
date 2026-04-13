# Test that taint flows through chained method calls where the receiver
# is itself a method call: get_data.strip should call get_data() first.

class Controller
  def show
    # ruleid: test-ruby-chained-method
    sink(get_data.strip)
  end

  def get_data
    source()
  end
end

def source()
  "tainted"
end

def sink(x)
end
