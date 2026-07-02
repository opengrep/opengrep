module Inner
  # Chain leaf.  process must reach App's MRO via Outer's include.
  def process(data)
    # ruleid: include-module-includes-module
    sink(data)
  end
end

def sink(x)
  puts x
end
