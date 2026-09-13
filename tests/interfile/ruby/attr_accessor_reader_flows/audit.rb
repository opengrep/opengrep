class Audit
  def record(value)
    # ruleid: attr-accessor-reader-flows
    sink(value)
  end
end
