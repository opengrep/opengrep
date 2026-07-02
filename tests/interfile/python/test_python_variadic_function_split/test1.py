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


