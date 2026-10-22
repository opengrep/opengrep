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

def empty_else_keeps_the_case(k)
  y = 0
  r = case k
      when 1 then y = taint_source()
      else
      end
  # ruleid: taint-switch-case-ruby
  sink(y)
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
