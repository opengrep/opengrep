class User
  def store(data)
    # ruleid: autoload-path-needs-no-require
    sink(data)
  end
end
