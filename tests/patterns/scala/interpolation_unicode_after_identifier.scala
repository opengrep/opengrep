object Foo {
  def f(user: String, amount: Int) = {
    //ERROR:
    sink(s"$user’s profile")
    //ERROR:
    sink(s"$user→home")
    //ERROR:
    sink(s"Congrats $user🎉")
    //ERROR:
    sink(s"欢迎$user，再见")
    //OK:
    sink(s"$amount€ total")
    // a letter continues the identifier, which is userä
    //OK:
    sink(s"$userä")
  }
}
