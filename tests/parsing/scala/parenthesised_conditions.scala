object ParenthesisedConditions:
  def thenAfterParens(a: Int) = if (a > 0) then 1 else 2

  def continued(a: Int, b: Int) = if (a > 0) && (b > 0) then 1 else 2

  def selection(xs: List[Int]) = if (xs).isEmpty then 0 else 1

  def thenOnNextLine(a: Int) =
    if (a > 0)
    then 1
    else 2

  def whileDo(n: Int) =
    var x = n
    while (x > 0) do x -= 1
    while (x < 0) && (x > -10) do x += 1

  def scala2(a: Int) =
    if (a > 0) a else -a

  def scala2ThenMember(ok: Boolean) = {
    if (ok) log(1)
    promise.then(2)
    if (ok) promise.then(3)
  }

  def scala2DoWhileAfterWhile(a: Boolean, d: Boolean) = {
    while (a) {
      b()
    }
    do {
      c()
    } while (d)
    while (a) b()
    println(1)
    do { c() } while (d)
  }

  def scala2DoWhileAsWhileBody(x: Boolean, n: Int, m: Int) = {
    var i = 0
    while (x)
      do { i += 1 } while (i < n)
    while (i < n) do {
      i += 1
    } while (i < m)
  }

  def scala2DoWhileAsForBody(n: Int) = {
    var j = 0
    for (i <- 0 until n)
      do {
        j += 1
      } while (j < i)
  }
