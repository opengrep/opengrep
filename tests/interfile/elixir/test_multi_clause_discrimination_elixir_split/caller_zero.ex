defmodule Caller do
  def caller_zero() do
    DiscriminationTest.dispatch(0, source())
  end
end
