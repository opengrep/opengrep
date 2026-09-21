def m
  for i in [source()]
    # ruleid: for_loop
    sink(i)
  end

  for j in ["safe"]
    # ok: for_loop
    sink(j)
  end

  items = [source()]
  for k in items
    # ruleid: for_loop
    sink(k)
  end
end
