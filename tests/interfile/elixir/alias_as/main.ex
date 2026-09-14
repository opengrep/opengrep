defmodule App.Main do
  alias App.Impl, as: I

  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    I.greet(t)
  end
end
