# Elixir map-pattern destructuring in a lambda param produces
# G.ParamPattern(map-pattern). Taint from source() must route through
# run_cb's application of cb to its argument and bind onto `val`.

defmodule Test do
  def run_cb(cb, x), do: cb.(x)

  def test_map_destructure do
    run_cb(fn %{key: val} ->
      # ruleid: test-param-pattern-taint
      sink(val)
    end, source())
  end

  # Baseline: plain single-param lambda. Passes today.
  def test_plain do
    run_cb(fn v ->
      # ruleid: test-param-pattern-taint
      sink(v)
    end, source())
  end
end
