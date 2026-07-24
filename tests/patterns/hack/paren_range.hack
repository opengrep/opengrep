// A parenthesized operand keeps its own range tight: matching the inner
// "*" reports "2 * 2", not "(2 * 2)".
$x = (
// MATCH:
2 * 2
);

// The parens fold into the enclosing "+", not into the inner "*": matching
// the "*" still reports the tight "2 * 2".
$x = (
// MATCH:
2 * 2
) + 3;

// Matching the outer "*" includes the leading paren of its left operand,
// so the match starts at "(".
$x =
// MATCH:
(
2 + 2) * 3;

// The same left-edge fold, reached through a subscript base.
$x =
// MATCH:
(
$a)[0] * 3;

// ... through a call callee.
$x =
// MATCH:
(
$a)(1) * 3;

// ... and through a parenthesized ternary operand.
$x =
// MATCH:
(
$b ? $c : $d) * 3;
