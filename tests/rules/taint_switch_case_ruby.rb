LIMIT = 1
WORD = /a/

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

def pinned_slot_does_not_match
  x = taint_source()
  y = 1
  case [2, 0]
  in [^y, _]
    # ok: taint-switch-case-ruby
    sink(x)
  end
end

def constant_slot_does_not_match
  x = taint_source()
  case [2, 0]
  in [LIMIT, _]
    # ok: taint-switch-case-ruby
    sink(x)
  end
end

def constant_slot_matches
  x = taint_source()
  case [1, 0]
  in [LIMIT, _]
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end

def regexp_label_matches_a_string
  x = taint_source()
  case "abc"
  when /a/
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end

def regexp_constant_slot_matches_a_string
  x = taint_source()
  case ["abc", 0]
  in [WORD, _]
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end

def integer_label_matches_a_float
  x = taint_source()
  case 1.0
  in 1
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end

def hash_pattern_matches_a_bigger_hash
  x = taint_source()
  case {a: 1, b: 2}
  in {a: 1}
    # ruleid: taint-switch-case-ruby
    sink(x)
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
  # ruleid: taint-switch-case-ruby
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
    # ok: taint-switch-case-ruby
    sink(y)
  end
end

def empty_else_keeps_the_case(k)
  y = 0
  r = case k
      when 1 then y = taint_source()
      else
      end
  # ruleid: taint-switch-case-ruby
  sink(y)
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
  # ruleid: taint-switch-case-ruby
  sink(y)
end

def next_in_a_block_skips_the_rest_of_it(items)
  items.each do |i|
    y = taint_source()
    next
    # ok: taint-switch-case-ruby
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
    # ok: taint-switch-case-ruby
    sink(y)
  end
end

def next_in_a_block_keeps_the_other_path(items)
  items.each do |i|
    x = taint_source()
    next if i == 1
    # ruleid: taint-switch-case-ruby
    sink(x)
  end
end
