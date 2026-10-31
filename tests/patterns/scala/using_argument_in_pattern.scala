// A using argument in a pattern matches only a using argument.
object A {
  def f(x: Ctx) = {
    //ERROR:
    run(using x)
    //OK:
    run(x)
  }
}
