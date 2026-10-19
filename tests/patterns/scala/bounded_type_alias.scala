// The bound of an opaque type alias is dropped.
object Foo:
  //ERROR:
  opaque type Id <: Int = Int

  //ERROR:
  opaque type Count = Int

  //OK:
  opaque type Name = String
