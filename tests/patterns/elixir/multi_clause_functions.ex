defmodule MyModule do
  # Two-clause function: both clauses collapse into one definition.
  # Pattern matches once, at the first clause.
  #ERROR:
  def factorial(0) do
    1
  end
  def factorial(n) do
    n * factorial(n - 1)
  end

  # Three-clause function with when guards: still one match.
  #ERROR:
  def classify(x) when x > 0 do
    :positive
  end
  def classify(x) when x < 0 do
    :negative
  end
  def classify(_) do
    :zero
  end

  # Single-clause function: one match, same as before.
  #ERROR:
  def single_clause(x) do
    x + 1
  end
end
