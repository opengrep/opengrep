defmodule Handler do
  def f(x) do
    x
  end

  def g(x) do
    f(x)
  end
end

defmodule Other do
  def f(x) do
    x
  end
end
