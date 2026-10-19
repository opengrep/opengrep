object Foo {
  def length(start: Int, last: Int) =
    //ERROR:
    end - start

  def shrink(n: Int) =
    while (n > 0)
      //ERROR:
      end -= 1
}

object Bar:
  def f(x: Int) =
    x + 1
  //OK:
  end f
