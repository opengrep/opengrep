# `Dec::Widget` is a homonym with a `run` method of the same name.  Dispatch
# on the bare name alone cannot distinguish it from the real `Widget#run`.
# `Sub` (below) must resolve `run` through its type/MRO, not globally.
module Dec
  class Widget
    def run(x)
      # ok: reopened-class-inheritance
      benign(x)
    end
  end
end
