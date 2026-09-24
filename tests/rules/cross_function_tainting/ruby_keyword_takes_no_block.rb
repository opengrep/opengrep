def each_sep(x, sep: ", ")
  yield x
end

def each_plain(x)
  yield x
end

def each_sep_declared(x, sep: ", ", &blk)
  yield x
end

def sep_only(x, sep: ", ")
  # ok: ruby_keyword_takes_no_block
  sink(sep)
end

def sep_named(x, sep: ", ")
  # ruleid: ruby_keyword_takes_no_block
  sink(sep)
end

def go
  each_sep(source()) do |v|
    # ruleid: ruby_keyword_takes_no_block
    sink(v)
  end
  each_plain(source()) do |v|
    # ruleid: ruby_keyword_takes_no_block
    sink(v)
  end
  each_sep_declared(source()) do |v|
    # ruleid: ruby_keyword_takes_no_block
    sink(v)
  end
  sep_only(source())
  sep_named("safe", sep: source())
  nil
end
