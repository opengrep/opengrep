def foo([x,y]) do
  # ruleid: list_taint_rule
  sink(x)
  
  # ruleid: list_taint_rule
  sink(y)
end

def bar([x|y]) do
  # ruleid: list_taint_rule
  sink(x)
  
  # ruleid: list_taint_rule
  sink(y)
end
