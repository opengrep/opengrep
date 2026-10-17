fun f(k: Any) {
  // ERROR: match
  when (k) {
    in 1..5 -> { a() }
    else -> { b() }
  }

  when (k) {
    !in 1..5 -> { a() }
    else -> { b() }
  }

  when (k) {
    is String -> { a() }
    else -> { b() }
  }

  when (k) {
    FOO -> { a() }
    else -> { b() }
  }
}
