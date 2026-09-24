class A {
  private var f1: String
  private let f3 = "abc"
  private var f4: String
  var f5: String
  private let f7: String
  private var f8 = "abc"
  private static var f2: String = "abc"
  init() { f1 = "abc"; f4 = "abc"; f5 = "abc"; f7 = "abc"; self.f9 = "abc" }
  private var f9: String
  func other() { f4 = "zzz" }
  func m() {
    // ERROR: match
    let loc = "abc"; sink(0, loc)
    sink(1, f1)
    sink(2, A.f2)
    // ERROR: match
    sink(3, f3)
    sink(4, f4)
    sink(5, f5)
    // ERROR: match
    sink(7, f7)
    sink(8, f8)
    sink(9, self.f9)
  }
}
extension A {
  func ext() { sink(10, f1); sink(11, self.f3) }
}
