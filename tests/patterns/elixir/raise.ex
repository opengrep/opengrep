defmodule RaisePattern do
  def f1() do
    # ERROR:
    raise RuntimeError
  end

  def f2(x) do
    # ERROR:
    raise RuntimeError, message: x
  end

  def f3(e, st) do
    reraise e, st
  end

  def f4(r) do
    exit(r)
  end

  def f5(v) do
    throw(v)
  end
end
