object Foo {
  def f(user: String, amount: Int) = {
    //ERROR:
    sink(s"$user’s profile")
    //ERROR:
    sink(s"$user→home")
    //OK:
    sink(s"$amount€ total")
  }
}
