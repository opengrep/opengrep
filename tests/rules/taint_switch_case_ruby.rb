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
