require_relative 'source'
require_relative 'sink'
require_relative 'zz_main'
class TestController
  def show
    if continue_params[:to]
      # ruleid: test-ruby-array-access
      sink(continue_params[:to])
    end
  end

  def continue_params
    source()
  end
end
