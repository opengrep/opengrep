object Foo {
  //ERROR:
  val a = <div class="row">{sink(appId)}</div>

  //OK:
  val b = <div class="row">sink(appId)</div>
}
