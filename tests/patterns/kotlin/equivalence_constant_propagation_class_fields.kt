class A {
  private var f1: String
  private val f3 = "abc"
  private var f4: String
  var f5: String
  private val f7: String
  private var f8 = "abc"
  init { f1 = "abc"; f4 = "abc"; f5 = "abc"; f7 = "abc" }
  fun other() { f4 = "zzz" }
  fun m() {
    // ERROR: match
    val loc = "abc"; sink(0, loc)
    // ERROR: match
    sink(1, f1)
    // ERROR: match
    sink(3, f3)
    sink(4, f4)
    sink(5, f5)
    // ERROR: match
    sink(7, f7)
    // ERROR: match
    sink(8, f8)
    // ERROR: match
    sink(9, this.f1)
  }
  companion object {
    private var f2: String
    init { f2 = "abc" }
    // ERROR: match
    fun n() { sink(2, f2) }
  }
}
class B(private val p: String) {
  private var g1: String
  constructor(x: Int) : this("abc") { g1 = "abc" }
  // ERROR: match
  fun m() { sink(10, g1); sink(11, p) }
}
