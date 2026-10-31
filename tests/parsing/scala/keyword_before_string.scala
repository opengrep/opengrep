object Results:
  def r(c: Boolean) = if c then"" else" result"
  def s(c: Boolean) = if (c) "a" else"b"
  def t(c: Boolean) = if (c) s"a$c" else s"""b$c"""
