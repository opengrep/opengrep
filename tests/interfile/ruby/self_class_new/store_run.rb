class Store
  def run(data)
    self.class.new.save(data)
  end
end
