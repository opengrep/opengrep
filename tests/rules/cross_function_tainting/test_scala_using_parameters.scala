// A call fills no using parameter with its positional arguments: they go to
// the other parameters, whatever the position of the using clause.
object Using {
  def pick(using c: Ctx)(x: String): Unit =
    // ruleid: scala_using_parameters
    sink(x)

  def context(using c: Ctx)(x: String): Unit =
    // ok: scala_using_parameters
    sink(c)

  // an explicit using argument fills the using parameter
  def explicit(using c: Ctx): Unit =
    // ruleid: scala_using_parameters
    sink(c)

  def explicitFirst(using c: Ctx)(x: String): Unit =
    // ok: scala_using_parameters
    sink(x)

  // a using parameter without a name takes no positional argument either
  def anonymous(using Ctx)(x: String): Unit =
    // ruleid: scala_using_parameters
    sink(x)

  // an explicit using argument also fills a Scala 2 implicit parameter
  def implicitParam(implicit c: Ctx): Unit =
    // ruleid: scala_using_parameters
    sink(c)

  // a parameter `context` is not the using parameter without a name
  def nameClash(using Ctx)(context: String): Unit =
    // ok: scala_using_parameters
    sink(context)

  def call(): Unit = {
    given Ctx = mk()
    pick(source())
    context(source())
    explicit(using source())
    explicitFirst(using source())("x")
    anonymous(source())
    implicitParam(using source())
    nameClash(using source())("a")
  }
}
