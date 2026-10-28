// A keyword before a string is a keyword, and the string a literal.
object Results:
  //ERROR:
  def r(c: Boolean) = if (c) "a" else"b"
  //ERROR:
  def s(c: Boolean) = if c then"a" else "b"
  //OK:
  def t(c: Boolean) = if (c) "a" else"c"
