object Foo {
  def length(start: Int, last: Int) = {
    val x = 1
    //ERROR:
    end - start
  }

  def shrink(n: Int) = {
    //ERROR:
    var end = n
    //ERROR:
    end -= 1
    //ERROR:
    sink(end)
  }
}

object Bar:
  def f(x: Int) =
    x + 1
  //OK:
  end f
