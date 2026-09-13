class Audit
  def write(data)
    # ruleid: top-level-constant-from-nested-scope
    sink(data)
  end
end
