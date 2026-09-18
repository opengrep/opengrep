defmodule App.Mid do
  def pass(m) do
    App.Impl.leak(m)
  end
end
