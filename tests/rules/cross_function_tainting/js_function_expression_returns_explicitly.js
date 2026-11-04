// A function expression returns only what a return statement gives; an arrow
// with an expression body returns the expression.
function main() {
  const s = source();
  const f = function () { foo(s); };
  // ok: js_function_expression_returns_explicitly
  sink(f());
  const g = function () { return s; };
  // ruleid: js_function_expression_returns_explicitly
  sink(g());
  const h = () => s;
  // ruleid: js_function_expression_returns_explicitly
  sink(h());
}
