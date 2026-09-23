defmodule ReraisePattern do
  def f1(e, st) do
    # ERROR:
    reraise e, st
  end

  def f2(mod, attrs, st) do
    # ERROR:
    reraise mod, attrs, st
  end

  def f3() do
    raise RuntimeError
  end

  def f4(x) do
    raise RuntimeError, message: x
  end

  def f5(r) do
    exit(r)
  end
end
