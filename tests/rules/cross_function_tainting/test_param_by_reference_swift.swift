func set(_ x: inout String) { x = source() }
func two(a: inout String, b: String) { a = b }

func caller() {
  var s = ""
  set(&s)
  // ruleid: test-param-by-reference-swift
  sink(s)
  var t = ""
  two(a: &t, b: source())
  // ruleid: test-param-by-reference-swift
  sink(t)
}
