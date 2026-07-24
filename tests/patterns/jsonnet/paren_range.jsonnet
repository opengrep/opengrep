// A parenthesized operand keeps its own range tight: matching the inner
// "*" reports "2 * 2", not "(2 * 2)".
local a = (
// MATCH:
2 * 2
);

// The parens fold into the enclosing "+", not into the inner "*": matching
// the "*" still reports the tight "2 * 2".
local b = (
// MATCH:
2 * 2
) + 3;

// Matching the outer "*" includes the leading paren of its left operand,
// so the match starts at "(".
local c =
// MATCH:
(
2 + 2) * 3;

// The same left-edge fold, reached through a member-access base.
local d =
// MATCH:
(
x).y * 3;

// ... and through an index base.
local e =
// MATCH:
(
x)[0] * 3;

a + b + c + d + e
