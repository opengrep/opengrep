// Literal record { a: p, b: "literal" }:
//   - .a's value is the tainted parameter p.
//   - .b's value is the string literal "literal" (no taint, Bot subshape).
//
// We should report sink(obj.a) (parameter taint flows through .a) and
// not sink(obj.b) (the string literal is clean). Previously, projection
// of the clean .b field fell through to the parent cell's xtaint and
// produced a false positive on line 13.

function f(p) {
  const obj = { a: p, b: "literal" };
  // ruleid: test-clean-field-leak-js
  sink(obj.a);
  // ok: test-clean-field-leak-js
  sink(obj.b);
}
