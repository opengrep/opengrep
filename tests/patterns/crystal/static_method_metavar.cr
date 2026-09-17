class Worker
  # ERROR:
  def self.build(task)
    new(task)
  end

  def perform(task)
    task.run
  end
end
