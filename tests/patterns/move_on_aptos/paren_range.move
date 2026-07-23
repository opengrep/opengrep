module 0x1::paren_range {
  fun f() {

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

    // ... and through an array-access base.
    // MATCH:
    (
    a)[0] * 3;
  }
}
