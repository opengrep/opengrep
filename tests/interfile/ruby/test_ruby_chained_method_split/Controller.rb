require_relative 'source'
require_relative 'sink'
require_relative 'zz_main'
class Controller
  def show
    # ruleid: test-ruby-chained-method
    sink(get_data.strip)
  end

  def get_data
    source()
  end
end
