// `...` in a using clause matches any number of using arguments, also none,
// and no other argument.
object A {
  def g(a: Ctx, b: Log) = {
    //ERROR:
    run(using a, b)
    //ERROR:
    run(using a)
    //ERROR:
    run()
    //OK:
    run(a, b)
  }
}
