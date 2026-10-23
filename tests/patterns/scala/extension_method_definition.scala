object Foo:
  extension (s: String)
    //ERROR:
    def shout(n: Int): String = danger(s)
    //ERROR:
    def whisper(n: Int): String = danger(s)

  //OK:
  def plain(s: String, n: Int): String = danger(s)
