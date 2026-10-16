fun f(k: Any) {
  // ERROR: match
  when (k) {
    FOO -> { a() }
  }

  // An else branch is not a condition.
  when (k) {
    else -> { a() }
  }
}
