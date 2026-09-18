defmodule Test do
  def test_map_destructure do
    run_cb(fn %{key: val} ->
      # ruleid: test-param-pattern-taint
      sink(val)
    end, source())
  end
end
