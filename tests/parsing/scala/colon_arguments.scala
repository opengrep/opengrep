object ColonArguments:
  def block() =
    run:
      println("a")
      println("b")

  def afterArguments() =
    test("name"):
      val x = 1
      assert(x == 1)

  def lambda(xs: List[Int]) = xs.map: x =>
    val y = x + 1
    y * 2

  def lambdaParams(xs: List[Int]) = xs.foldLeft(0): (acc, x) =>
    acc + x

  def lambdaNoParams() = schedule: () =>
    println("tick")

  def underscore(xs: List[Int]) = xs.map:
    _ + 1

  def cases(xs: List[Option[Int]]) = xs.map:
    case Some(x) => x
    case None => 0

  def chained(xs: List[Int]) =
    xs
      .map: x =>
        x + 1
      .filter: x =>
        x > 0

  val rhs = Some:
    1

  def nested(xs: List[List[Int]]) = xs.map: ys =>
    ys.map: y =>
      y + 1

class SelfTypeInBraces { self: AnyRef =>
  def f = 1
}

object Scala2TypedLambda {
  val f = { x: Int =>
    x + 1
  }
}
