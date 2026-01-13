def test1(*args):

  for x in args.items():
      # ruleid: taint
      sink(x)

  # ok:
  sink(args[0])
  # ruleid: taint
  sink(args[1])

  # ok:
  sink(args[2])


test1("abc", source(), "xyz")


def test2(x, *args, y):

  # ruleid: taint
  sink(x)

  # ruleid: taint
  sink(y)

  # ok:
  sink(args)


test2(source(), "abc", "uvw", "xyz", y=source())
