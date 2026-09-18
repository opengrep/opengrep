defmodule TaintTest do
  def process_data(data) do
    # ruleid: simple_elixir_taint
    sink(data)
  end
end
