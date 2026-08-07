// A parenthesized operand keeps its own range tight: matching the inner
// "*" reports "2 * 2", not "(2 * 2)".
_ = (
// MATCH:
2 * 2
)

// The parens fold into the enclosing "+", not into the inner "*": matching
// the "*" still reports the tight "2 * 2".
_ = (
// MATCH:
2 * 2
) + 3

// Matching the outer "*" includes the leading paren of its left operand,
// so the match starts at "(".
_ =
// MATCH:
(
2 + 2) * 3

// The same left-edge fold, reached through a navigation base.
_ =
// MATCH:
(
a).b * 3

// ... through a call callee.
_ =
// MATCH:
(
a)(1) * 3

// ... and through a parenthesized ternary operand.
_ =
// MATCH:
(
b ? c : d) * 3
