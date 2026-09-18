defmodule App.Main do
  import App.Impl

  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    greet(t)
  end
end
