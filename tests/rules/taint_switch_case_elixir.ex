defmodule SwitchCase do
  def guarded_clause_does_not_fall_through(k) do
    y = 0
    case k do
      n when n > 0 ->
        y = taint_source()
      _ ->
        # ok: taint-switch-case-elixir
        sink(y)
    end
  end

  def failed_guard_reaches_later_clause(k) do
    x = taint_source()
    case k do
      n when n > 0 ->
        a(n)
      _ ->
        # ruleid: taint-switch-case-elixir
        sink(x)
    end
  end

  def guard_reads_the_binding() do
    k = taint_source()
    case k do
      n when is_integer(n) ->
        # ruleid: taint-switch-case-elixir
        sink(n)
      _ ->
        0
    end
  end
end
