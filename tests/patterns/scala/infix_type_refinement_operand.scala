object Foo {
  //ERROR:
  type X = A ^ { type B }

  //OK:
  type Y = A ^ { type C }

  //OK:
  type Z = A^{b}
}
