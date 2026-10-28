// A comment, a character data section or a processing instruction starts an
// XML literal, which is not a call.
object Markup {
  //OK:
  val note = <!-- a comment -->
  //OK:
  val data = <![CDATA[x < y]]>
  //OK:
  val doc = <?xml version="1.0"?>
    <root>{body}</root>
  //ERROR:
  val other = f(y)
}
