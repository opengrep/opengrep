class Job
  def run
    handle(source())
  end

  def handle(data)
    # ruleid: implicit-self-calls-own-method
    sink(data)
  end
end
