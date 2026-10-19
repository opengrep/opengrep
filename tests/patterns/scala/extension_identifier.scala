object Foo {
  def f(extension: Ext) = {
    //ERROR:
    extension.journalFor(null)
  }

  def g(ext: Ext) = {
    //OK:
    ext.snapshotStoreFor(null)
  }

  // a Scala 2 call of extension, not an extension
  def h(extension: Ext) = {
    //ERROR:
    extension.journalFor(null)
    extension (0)
  }
}
