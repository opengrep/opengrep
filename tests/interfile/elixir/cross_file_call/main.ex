defmodule App.Main do
  def source, do: System.get_env("SECRET")

  def run do
    tainted = source()
    App.Impl.greet(tainted)
  end
end
