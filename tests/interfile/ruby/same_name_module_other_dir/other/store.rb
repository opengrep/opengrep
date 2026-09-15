# The constant path of this class is Other::Store, so a reference to Store
# from sub/app.rb never names it.
module Other
  class Store
    def save(data)
      # ok: same-name-module-other-dir
      sink(data)
    end
  end
end
