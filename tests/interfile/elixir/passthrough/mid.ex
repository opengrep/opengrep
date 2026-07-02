defmodule App.Mid do
  def relay(m) do
    App.Impl.leak(m)
  end
end
