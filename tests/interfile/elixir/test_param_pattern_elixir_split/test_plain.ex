defmodule Test do
  def test_plain do
    run_cb(fn v ->
      # ruleid: test-param-pattern-taint
      sink(v)
    end, source())
  end
end
