# ...and REOPENED here with the sink method.  `Widget`'s methods are the UNION
# across both files; a subclass must inherit `run` from this reopening even
# though `describe` lives in the other.
class Widget
  def run(x)
    # ruleid: reopened-class-inheritance
    sink(x)
  end
end
