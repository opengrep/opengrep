class Create
  def run
    User.new.store(source())
  end
end
