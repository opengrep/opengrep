object Foo:
  //ERROR:
  test("a"):
    check(1)

  //OK:
  test("b")
  check(2)
