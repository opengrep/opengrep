fun f(k: Int) {
  // ERROR: match
  when (k) {
    1, 2, 5 -> a()
  }

  // ERROR: match
  when (k) {
    g(2) -> a()
  }

  when (k) {
    5, 6 -> a()
  }
}
