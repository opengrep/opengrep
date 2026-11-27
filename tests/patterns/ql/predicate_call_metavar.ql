predicate f(int x) {
  // ERROR:
  p(x)
}

predicate g(int x) {
  // ERROR:
  M::p(x)
}

predicate h(int x) {
  x = 1 or x = 2
}
