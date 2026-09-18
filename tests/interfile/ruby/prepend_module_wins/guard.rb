module Guard
  def handle(data)
    # ruleid: prepend-module-wins
    sink(data)
  end
end
