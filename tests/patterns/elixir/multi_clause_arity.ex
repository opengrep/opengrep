defmodule MyModule do
  # foo/1 — two clauses, matches once.
  #ERROR:
  def foo(0) do
    0
  end
  def foo(x) do
    x * 2
  end

  # foo/2 — two clauses, also matches once (separate from foo/1).
  #ERROR:
  def foo(0, y) do
    y
  end
  def foo(x, y) do
    x + y
  end
end
