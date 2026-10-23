object Foo:
  extension (s: String)
    //ERROR:
    def shout(n: Int): String = s * n
    //OK:
    def whisper(m: Long): String = s

  //ERROR:
  def plain(n: Int): String = "a" * n
