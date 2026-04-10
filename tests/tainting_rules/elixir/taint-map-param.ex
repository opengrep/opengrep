defmodule TaintMapParam do
  # keyword syntax, single clause
  def keyword_single(%{user: user}) do
    # ruleid: taint-map-param
    sink(user)
  end

  # arrow syntax, single clause
  def arrow_single(%{"user" => user}) do
    # ruleid: taint-map-param
    sink(user)
  end

  # multi-clause, keyword
  def multi_kw(%{a: a}) do
    # ruleid: taint-map-param
    sink(a)
  end

  def multi_kw(%{b: b}) do
    # ruleid: taint-map-param
    sink(b)
  end

  # multi-clause, arrow
  def multi_arrow(%{"a" => a}) do
    # ruleid: taint-map-param
    sink(a)
  end

  def multi_arrow(%{"b" => b}) do
    # ruleid: taint-map-param
    sink(b)
  end

  # plain param (not map), single clause
  def plain(input) do
    # ruleid: taint-map-param
    sink(input)
  end

  # safe: variable not from param
  def safe(%{user: _user}) do
    other = "safe"
    # ok: taint-map-param
    sink(other)
  end
end
