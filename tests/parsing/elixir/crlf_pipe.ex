defmodule Pipe do
  def run(x) do
    x
    |> Enum.map(&String.upcase/1)
    |> Enum.join(",")
  end
end
