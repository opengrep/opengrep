defmodule TaintTest do
  def get_tainted_data() do
    source()
  end

  def process_data(data) do
    # ruleid: simple_elixir_taint
    sink(data)
  end

  def main() do
    tainted_input = get_tainted_data()
    result = process_data(tainted_input)
    result
  end
end

TaintTest.main()