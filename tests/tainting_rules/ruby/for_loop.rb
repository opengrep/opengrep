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

  for a, b in [[source(), 1]]
    # ruleid: for_loop
    sink(a)
    # todook: for_loop
    sink(b)
  end

  for c, d in [[1, source()]]
    # todook: for_loop
    sink(c)
    # ruleid: for_loop
    sink(d)
  end
end
