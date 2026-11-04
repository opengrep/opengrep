// A function expression reads the fields of the object it is called on:
// the receiver of a method call, or, when a function the analysis does not
// see calls it, the this of the function that defines it.
class A {
  bound() {
    this.y = source();
    const g = function () {
      // ruleid: js_function_expression_this_is_the_receiver
      sink(this.y);
    }.bind(this);
    g();
  }
  each() {
    this.x = source();
    [1].forEach(function () {
      // ruleid: js_function_expression_this_is_the_receiver
      sink(this.x);
    }, this);
  }
}
function literal() {
  const o = { x: source(), m: function () { return this.x; } };
  // ruleid: js_function_expression_this_is_the_receiver
  sink(o.m());
  const p = { x: "safe", m: function () { return this.x; } };
  // ok: js_function_expression_this_is_the_receiver
  sink(p.m());
}
function Ctor() {
  this.z = source();
  this.show = function () {
    // ruleid: js_function_expression_this_is_the_receiver
    sink(this.z);
  };
  this.show();
}
