def joins(*xs, sep: ", ")
  # ruleid: ruby_keyword_after_rest
  sink(xs)
end

def seps(*xs, sep: ", ")
  # ok: ruby_keyword_after_rest
  sink(sep)
end

def named(*xs, sep: ", ")
  # ruleid: ruby_keyword_after_rest
  sink(sep)
end

def keyword_then_block(*xs, sep: ", ", &blk)
  yield xs
end

def go
  joins("safe", source())
  seps("safe", source())
  named("safe", sep: source())
  keyword_then_block("safe", source()) do |v|
    # ruleid: ruby_keyword_after_rest
    sink(v)
  end
  nil
end
