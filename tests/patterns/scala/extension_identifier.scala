object Foo {
  def f(extension: Ext) = {
    //ERROR:
    extension.journalFor(null)
  }

  def g(ext: Ext) = {
    //OK:
    ext.snapshotStoreFor(null)
  }
}
