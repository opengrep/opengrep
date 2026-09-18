defmodule App.Main do
  import App.Impl, only: [greet: 1]

  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    greet(t)
    other(t)
  end
end
