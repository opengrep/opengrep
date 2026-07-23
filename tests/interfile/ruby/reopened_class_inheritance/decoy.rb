# A homonym `Dec::Widget` with a same-named `run` — a benign collision that
# leaf-name method dispatch cannot tell apart from the real `Widget#run`.
# `Sub` (below) must resolve `run` through its type/MRO, not globally.
module Dec
  class Widget
    def run(x)
      # ok: reopened-class-inheritance
      benign(x)
    end
  end
end
