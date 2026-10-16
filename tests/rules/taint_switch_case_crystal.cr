CMD = 7
B = 2
RED = 3

def test(mode)
  q = "safe"
  # ruleid: switch-case-constant-propagation-crystal
  exec(CMD)
  case mode
  when CMD
    # ruleid: switch-case-constant-propagation-crystal
    exec(CMD)
  when B
    q = taint_source()
    # ruleid: taint-switch-case-crystal
    sink(q)
  end
  # ruleid: switch-case-constant-propagation-crystal
  exec(CMD)
end

def label_is_not_bound
  y = taint_source()
  case y
  when RED
    nil
  end
  # ok: taint-switch-case-crystal
  sink(RED)
  # ruleid: taint-switch-case-crystal
  sink(y)
end

def exhaustive_case(mode)
  case mode
  in String
    nil
  in Int32
    # ruleid: taint-switch-case-crystal
    sink(taint_source())
  end
end

def condition_only_case(condition)
  case
  when condition
    # ruleid: taint-switch-case-crystal
    sink(taint_source())
  end
end
