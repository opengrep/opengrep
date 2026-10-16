const val FOO = 1
const val BAR = 2

fun branchesDoNotFallThrough(k: Int) {
  var y = 0
  when (k) {
    FOO -> { y = taint_source() }
    BAR -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun branchesOfAnExpressionDoNotFallThrough(k: Int): Int {
  var y = 0
  val r = when (k) {
    FOO -> { y = taint_source(); 1 }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
      2
    }
  }
  return r
}

fun laterBranchKeepsTaint(k: Int) {
  val x = taint_source()
  when (k) {
    FOO -> { a() }
    BAR -> {
      // ruleid: taint-switch-case-kotlin
      sink(x)
    }
  }
}

fun conditionsSeparatedByCommas(k: Int) {
  var y = 0
  when (k) {
    1, 2 -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun typeTest(k: Any) {
  var y = 0
  when (k) {
    is String -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun negatedTypeTest(k: Any) {
  var y = 0
  when (k) {
    !is Int -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun rangeTest(k: Int) {
  var y = 0
  when (k) {
    in 1..5 -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun negatedRangeTest(k: Int) {
  var y = 0
  when (k) {
    !in 1..5 -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun whenWithoutSubject(n: Int) {
  var y = 0
  when {
    n > 1 -> { y = taint_source() }
    else -> {
      // ok: taint-switch-case-kotlin
      sink(y)
    }
  }
}

fun emptyArmKeepsTheWhen(k: Int) {
  var y = 0
  when (k) {
    1 -> { y = taint_source() }
    else -> {}
  }
  // ruleid: taint-switch-case-kotlin
  sink(y)
}
