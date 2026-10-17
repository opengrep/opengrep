fun f(k: Any) {
  // ERROR: match
  when (k) {
    in 1..5 -> { a() }
    else -> { b() }
  }

  // ERROR: match
  when (k) {
    5 -> { a() }
    else -> { b() }
  }

  when (k) {
    in 1..4 -> { a() }
    else -> { b() }
  }

  // ERROR: match
  when (k) {
    4, 5 -> { a() }
    else -> { b() }
  }
}
