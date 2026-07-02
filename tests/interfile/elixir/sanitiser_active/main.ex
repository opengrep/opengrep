defmodule App.Main do
  def source, do: System.get_env("SECRET")

  def run do
    x = source()
    y = App.Mid.process(x)
    App.Impl.emit(y)
  end
end
