defmodule M do
  def f(x) do
    (a = 1; b = x)
    foo(a, b)
    if x do
      c = 1
    end
    foo(c)
  end
end
