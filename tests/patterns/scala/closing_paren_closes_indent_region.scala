object Foo {
  def f(xs: List[Int]) = {
    //ERROR:
    xs.foreach(x =>
      if (x > 0)
        println(x))
  }

  def g(xs: List[Int]) = {
    //OK:
    xs.foreach(x => println(x))
  }
}
