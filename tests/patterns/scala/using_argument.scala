// An argument in a pattern also matches a using argument.
object A {
  def f(x: Ctx) = {
    //ERROR:
    run(using x)
    //ERROR:
    run(x)
  }
}
