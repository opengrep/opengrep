object Foo {
  def f(df: DataFrame) = {
    //ERROR:
    df.select(inline(array()))
    //ERROR:
    val r = inline(array())
    //OK:
    val s = array()
    r
  }
}
