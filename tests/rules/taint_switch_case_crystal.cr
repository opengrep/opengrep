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

def regexp_label_matches_a_string
  x = taint_source()
  case "abc"
  when /a/
    # ruleid: taint-switch-case-crystal
    sink(x)
  end
end

def integer_label_matches_a_float
  x = taint_source()
  case 1.0
  when 1
    # ruleid: taint-switch-case-crystal
    sink(x)
  end
end

def integer_label_does_not_match_another_integer
  x = taint_source()
  case 2
  when 1
    # ok: taint-switch-case-crystal
    sink(x)
  end
end
