def foo()
  src = source()

  # testing that this is not considered always returning
  src or return
  
  # ruleid: taint
  sink(src)
end
