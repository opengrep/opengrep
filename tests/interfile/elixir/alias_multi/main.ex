defmodule App.Main do
  alias App.{Impl, Mid}

  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    Impl.greet(t)
    Mid.pass(t)
  end
end
