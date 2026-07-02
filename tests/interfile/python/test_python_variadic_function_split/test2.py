def test2(x, *args, y):

  # ruleid: taint
  sink(x)

  # ruleid: taint
  sink(y)

  # ok:
  sink(args)


