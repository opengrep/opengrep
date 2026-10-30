// A function called with no receiver has no this: it does not read the
// fields of the instance whose method defines it.
class A {
  constructor() {
    this.f = source();
  }
  declared() {
    function inner() { return this.f; }
    // ok: js_bare_call_has_no_this
    sink(inner());
  }
  expression() {
    const g = function () { return this.f; };
    // ok: js_bare_call_has_no_this
    sink(g());
  }
  method() {
    // ruleid: js_bare_call_has_no_this
    sink(this.f);
  }
}
