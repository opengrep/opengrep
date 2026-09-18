defmodule App.Main do
  alias App.Impl

  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    Impl.greet(t)
  end
end
