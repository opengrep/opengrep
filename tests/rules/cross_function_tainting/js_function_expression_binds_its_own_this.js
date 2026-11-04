// An arrow function takes the this of the method that defines it; a function
// expression and an object-literal method bind their own.
class A {
  arrow() {
    this.v = source();
    const f = () => this.v;
    // ruleid: js_function_expression_binds_its_own_this
    sink(f());
  }
  expression() {
    this.v = source();
    const f = function () { return this.v; };
    // ok: js_function_expression_binds_its_own_this
    sink(f());
  }
  method() {
    this.v = source();
    const o = { v: "safe", get() { return this.v; } };
    // ok: js_function_expression_binds_its_own_this
    sink(o.get());
  }
}
