defmodule ExitPattern do
  def f1(reason) do
    # ERROR:
    exit(reason)
  end

  def f2() do
    raise RuntimeError
  end

  def f3(e, st) do
    reraise e, st
  end

  def f4(v) do
    throw(v)
  end
end
