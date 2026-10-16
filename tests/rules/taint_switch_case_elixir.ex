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

  def pinned_slot_does_not_match() do
    x = taint_source()
    y = 1
    case {2, 0} do
      {^y, _} ->
        # ok: taint-switch-case-elixir
        sink(x)
      _ ->
        0
    end
  end

  def pinned_slot_matches() do
    x = taint_source()
    y = 2
    case {2, 0} do
      {^y, _} ->
        # ruleid: taint-switch-case-elixir
        sink(x)
      _ ->
        0
    end
  end

  def pin_in_a_match_binds_nothing(foo) do
    ^foo = taint_source()
    # ok: taint-switch-case-elixir
    sink(foo)
  end

  def later_map_clause_is_reachable(m) do
    x = taint_source()
    case m do
      %{a: a} ->
        g(a)
      %{b: _b} ->
        # ruleid: taint-switch-case-elixir
        sink(x)
    end
  end

  def map_pattern_matches_a_bigger_map() do
    x = taint_source()
    case %{a: 1, b: 2} do
      %{a: 1} ->
        # ruleid: taint-switch-case-elixir
        sink(x)
      _ ->
        0
    end
  end

  def empty_map_pattern_matches_any_map() do
    x = taint_source()
    case %{a: 1} do
      %{} ->
        # ruleid: taint-switch-case-elixir
        sink(x)
      _ ->
        0
    end
  end
end
