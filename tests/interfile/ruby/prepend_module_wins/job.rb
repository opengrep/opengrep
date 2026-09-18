class Job
  prepend Guard

  def handle(data)
    # ok: prepend-module-wins
    sink(data)
  end
end
