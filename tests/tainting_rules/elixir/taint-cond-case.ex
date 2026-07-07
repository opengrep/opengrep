def foo() do
  x = source()
  cond do
    # ruleid: cond_taint_rule
    y = sink(x) -> 3
  end
end

# issue #644: an assignment in a cond branch condition is evaluated in
# sequence, so its taint must reach later branches.
def issue_644() do
  cond do
    y = source() -> 3
    # ruleid: cond_taint_rule
    true -> sink(y)
  end
end

def issue_644_no_source() do
  cond do
    y = safe() -> 3
    # ok: cond_taint_rule
    true -> sink(y)
  end
end

# A source in a LATER branch condition must not taint an EARLIER branch's
# body: cond conditions are evaluated top-to-bottom, so on the path that
# runs `sink(y)` the assignment `y = source()` has not executed yet.
def sink_before_source() do
  cond do
    # ok: cond_taint_rule
    guard() -> sink(y)
    y = source() -> 3
  end
end
