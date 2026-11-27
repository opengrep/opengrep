predicate f(int a) {
  // ERROR:
  A::B::c(a)
  or
  B::A::c(a)
  or
  A::c(a)
}
