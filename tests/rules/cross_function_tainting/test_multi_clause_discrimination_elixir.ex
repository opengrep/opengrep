defmodule DiscriminationTest do
  # Two clauses, both with a sink on the second argument. The first
  # arg discriminates: clause 1 matches literal 0, clause 2 matches
  # anything else.

  def dispatch(0, x) do
    # ruleid: test-multi-clause-discrimination-elixir
    sink(x)
  end
  def dispatch(_n, x) do
    # ok: test-multi-clause-discrimination-elixir
    sink(x)
  end

  # Calls dispatch with literal 0 as the discriminator and a tainted
  # source as the second arg. Only clause 1 matches at the call site,
  # so only clause 1's sink should fire. Clause 2's effect has cond
  # NOT(__p0__ == 0) AND ... which folds to false here -> dropped by
  # classify_guards.
  def caller_zero() do
    dispatch(0, source())
  end
end
