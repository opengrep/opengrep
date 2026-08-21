defmodule App.Main do
  def source, do: System.get_env("SECRET")

  def run do
    t = source()
    App.Mid.relay(t)
  end
end
