object TextExtensions:
  extension (text: String)
    //ERROR:
    def write(count: Int): Unit = sink(text)

    //OK:
    def writeLocal(count: Int): Unit = sink(local)
