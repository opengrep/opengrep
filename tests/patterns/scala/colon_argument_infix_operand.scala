// A colon argument can be the right operand of an infix operator, as in
// ScalaTest's `"a" should "b" in:`.
class Spec extends AnyFlatSpec:
  //ERROR:
  "a" should "b" in:
    sink(source())

  //ERROR:
  xs foreach: x =>
    sink(x)

  //OK:
  val n = x foo: Int
