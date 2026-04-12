
defmodule TaintStructParam do
  # keyword syntax, single clause
  def keyword_single(%Some_struct{user: user}) do
    # ruleid: taint-struct-param
    sink(user)
  end

  # arrow syntax, single clause
  def arrow_single(%Some_struct{"user" => user}) do
    # ruleid: taint-struct-param
    sink(user)
  end

  # multi-clause, keyword
  def multi_kw(%Some_struct{a: a}) do
    # ruleid: taint-struct-param
    sink(a)
  end

  def multi_kw(%Some_struct{b: b}) do
    # ruleid: taint-struct-param
    sink(b)
  end

  # multi-clause, arrow
  def multi_arrow(%Some_struct{"a" => a}) do
    # ruleid: taint-struct-param
    sink(a)
  end

  def multi_arrow(%Some_struct{"b" => b}) do
    # ruleid: taint-struct-param
    sink(b)
  end

  # plain param (not map), single clause
  def plain(input) do
    # ruleid: taint-struct-param
    sink(input)
  end

  # safe: variable not from param
  def safe(%Some_struct{user: _user}) do
    other = "safe"
    # ok: taint-struct-param
    sink(other)
  end
end
