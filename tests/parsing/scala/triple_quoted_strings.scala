object TripleQuotedStrings {
  val fourQuotes = """SELECT R"\n""""
  val backslashAtEnd = """\1234\"""
  val rawBackslashAtEnd = raw"""\1234\"""
  val interpolatedBackslash = s"""a\"b $fourQuotes"""
}
