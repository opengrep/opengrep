function foo() {

  // A parenthesized operand keeps its own range tight: matching the inner
  // "*" reports "2 * 2", not "(2 * 2)".
  (
  // MATCH:
  2 * 2
  );

  // The parens fold into the enclosing "+", not into the inner "*": matching
  // the "*" still reports the tight "2 * 2".
  (
  // MATCH:
  2 * 2
  ) + 3;

  // Matching the outer "*" includes the leading paren of its left operand,
  // so the match starts at "(".
  // MATCH:
  (
  2 + 2) * 3;

  // The same left-edge fold, reached through a member-access base.
  // MATCH:
  (
  a).b * 3;

  // ... through a call callee.
  // MATCH:
  (
  a)(1) * 3;

  // ... through an array-access base.
  // MATCH:
  (
  a)[0] * 3;

  // ... and through a parenthesized ternary operand.
  // MATCH:
  (
  b ? c : d) * 3;
}
