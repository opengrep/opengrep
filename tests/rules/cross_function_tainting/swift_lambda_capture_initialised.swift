// The initialiser is evaluated where the closure is created.
func copiesTheTaintedValue() {
  let a = source()
  let f = { [y = a] in
    // ruleid: swift_lambda_capture_initialised
    sink(y)
  }
  f()
}

// The initialised capture is the closure's own variable.
func shadowsTheTaintedVariable() {
  let x = source()
  let f = { [x = 0] in
    // ok: swift_lambda_capture_initialised
    sink(x)
  }
  f()
}
