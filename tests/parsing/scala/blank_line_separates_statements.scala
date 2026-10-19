object BlankLineSeparatesStatements {
  case class Foo2(a: Int, b: Int)

  (Eq[Int], Eq[Int]).contramap2((f: Foo2) => (f.a, f.b))
}

trait SharedPluginSpec {
  val system: ActorSystem

  {
    import scala.concurrent.duration._
    println(1.second)
  }
}

object ExtensionAfterBlankLine:
  extension (x: Int)

    def double = x * 2

    def triple = x * 3
