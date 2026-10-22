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

def break_leaves_the_loop(k)
  y = 0
  while more()
    case k
    when 1
      y = taint_source()
      break
    end
    y = 0
  end
  # ruleid: taint-switch-case-crystal
  sink(y)
end

def break_skips_the_rest_of_the_loop(k)
  while more()
    y = 0
    case k
    when 1
      y = taint_source()
      break
    end
    # ok: taint-switch-case-crystal
    sink(y)
  end
end

def break_in_a_block_leaves_the_block(items)
  y = 0
  items.each do |i|
    case i
    when 1
      y = taint_source()
      break
    end
    y = 0
  end
  # ruleid: taint-switch-case-crystal
  sink(y)
end

def next_in_a_block_skips_the_rest_of_it(items)
  items.each do |i|
    y = taint_source()
    next
    # ok: taint-switch-case-crystal
    sink(y)
  end
end

def break_in_a_block_skips_the_rest_of_it(items)
  items.each do |i|
    y = 0
    case i
    when 1
      y = taint_source()
      break
    end
    # ok: taint-switch-case-crystal
    sink(y)
  end
end

def next_in_a_block_keeps_the_other_path(items)
  items.each do |i|
    x = taint_source()
    next if i == 1
    # ruleid: taint-switch-case-crystal
    sink(x)
  end
end
