def later_branch_keeps_taint(k)
  x = taint_source()
  case k
  in FOO
    a()
  in BAR
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end

def label_is_not_bound
  k = taint_source()
  case k
  in FOO
    # ok: taint-switch-case-ruby
    sink(FOO)
  end
end

def guarded_branch_does_not_fall_through(k)
  y = 0
  case k
  in Integer => n if n > 0
    y = taint_source()
  in _
    # ok: taint-switch-case-ruby
    sink(y)
  end
end

def failed_guard_reaches_later_branch(k)
  x = taint_source()
  case k
  in Integer => n if n > 0
    a(n)
  in _
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end
