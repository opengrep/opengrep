# Elixir cons-pattern destructure [h | t] / [a, b | t]: the tail
# binding receives positions from k onwards of the incoming list. A
# source at any position >= k reaches the tail binding; a source at
# a fixed slot before [|] does not.

defmodule M do
  def handler_head([head | _]) do
    # ruleid: test-cons-elixir
    sink(head)
  end

  def caller_head() do
    handler_head([source(), "ok"])
  end


  def handler_tail([_ | tail]) do
    # ruleid: test-cons-elixir
    sink(tail)
  end

  def caller_tail() do
    handler_tail(["safe", source()])
  end


  def handler_tail_deep([_ | tail]) do
    # ruleid: test-cons-elixir
    sink(tail)
  end

  def caller_tail_deep() do
    # source four positions into the tail range
    handler_tail_deep(["safe", "a", "b", "c", "d", source()])
  end


  def handler_clean_head([head | _]) do
    # ok: test-cons-elixir
    sink(head)
  end

  def caller_clean_head() do
    handler_clean_head(["safe", "ok"])
  end


  def handler_tail_source_in_head([_ | tail]) do
    # ok: test-cons-elixir
    sink(tail)
  end

  def caller_tail_source_in_head() do
    # source goes to head; tail covers positions [1..]
    handler_tail_source_in_head([source(), "ok"])
  end


  # Two-fixed-slots form [a, b | t]; t covers positions [2..]

  def handler_t_pos2([_a, _b | t]) do
    # ruleid: test-cons-elixir
    sink(t)
  end

  def caller_t_pos2() do
    handler_t_pos2(["safe", "ok", source()])
  end


  def handler_t_deep([_a, _b | t]) do
    # ruleid: test-cons-elixir
    sink(t)
  end

  def caller_t_deep() do
    handler_t_deep(["safe", "ok", "x", "y", source()])
  end


  def handler_t_source_in_b([_a, _b | t]) do
    # ok: test-cons-elixir
    sink(t)
  end

  def caller_t_source_in_b() do
    # source at position 1 binds [_b]; [t] covers positions [2..]
    handler_t_source_in_b(["safe", source(), "ok"])
  end
end
