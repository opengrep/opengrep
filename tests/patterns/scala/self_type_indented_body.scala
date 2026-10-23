trait A {
  //OK:
  self: B =>
    //ERROR:
    def f = run(1)
}
