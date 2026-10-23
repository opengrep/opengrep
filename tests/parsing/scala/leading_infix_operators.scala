object LeadingInfixOperators:
  def conjunction(a: Boolean, b: Boolean, c: Boolean) =
    a
    && b
    || c

  def condition(a: Boolean, b: Boolean) =
    if a
      && b
    then 1
    else 2

  def sum(a: Int, b: Int) =
    val s =
      a
      + b
    s

  def ownLine(a: Int, b: Int) =
    a
    +
    b

  def thenBranch(x: Boolean, a: Int, b: Int) =
    if x then
        a
      + b
    else 0

  def backquoted(xs: List[Int], ys: List[Int]) =
    xs
    `++` ys
