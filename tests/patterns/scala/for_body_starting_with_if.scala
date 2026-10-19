object Foo {
  def f(xs: List[Int]) = {
    //ERROR:
    for (x <- xs) if (x > 0) sink(x) else log(x)
  }

  def g(xs: List[Int]) = {
    //OK:
    for (x <- xs; if x > 0) log(x)
  }
}
