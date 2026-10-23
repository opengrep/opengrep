object ExtensionIdentifier {
  def call(extension: Ext) = {
    extension.start()
    extension should be(null)
  }

  def lambda(xs: List[Ext]) = xs.foreach { extension => println(extension) }

  def pattern(x: Any) = x match {
    case extension: Ext => extension.start()
  }

  def interpolation(extension: String) = s"${extension}"

  extension (x: Int) def double = x * 2
}
