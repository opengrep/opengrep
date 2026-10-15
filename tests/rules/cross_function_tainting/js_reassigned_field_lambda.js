// A field holds the lambda assigned last: the constructor replaces the
// initialiser.
class C {
  f = (x) => "safe";
  constructor() { this.f = (x) => x; }
  m() {
    // ruleid: js_reassigned_field_lambda
    sink(this.f(source()));
  }
}
