# The constant path of this class is Other::Widget, so a reference to Widget
# from app.rb never names it, whatever app.rb requires.
module Other
  class Widget
    def process(x)
      # ok: homonym-class-require
      sink(x)
    end
  end
end
