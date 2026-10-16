fun f(k: Any) {
  // ERROR: match
  when (k) {
    FOO -> { a() }
    else -> { b() }
  }

  // ERROR: match
  when (k) {
    is String -> { a() }
    else -> { b() }
  }

  // ERROR: match
  when (k) {
    in 1..5 -> { a() }
    else -> { b() }
  }

  // ERROR: match
  when (k) {
    1, 2 -> { a() }
    else -> { b() }
  }
}
