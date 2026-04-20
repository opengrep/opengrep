defmodule CaretExample do
  def check(x) do
    y = 42
    # ruleid: find-caret
    case x do
      ^y -> :match
      _ -> :no_match
    end
  end
end
