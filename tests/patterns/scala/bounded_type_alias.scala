object Foo:
  //ERROR:
  opaque type Id <: Int = Int

  //OK:
  opaque type Name = String
