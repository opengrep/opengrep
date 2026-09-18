defmodule TaintTest do
  def main() do
    tainted_input = get_tainted_data()
    result = process_data(tainted_input)
    result
  end
end
