module 0x1::paren_range {
  fun f() {

    // A parenthesized operand keeps its own range tight: matching the inner
    // "*" reports "2 * 2", not "(2 * 2)".
    let a = (
    // MATCH:
    2 * 2
    );

    // The parens fold into the enclosing "+", not into the inner "*": matching
    // the "*" still reports the tight "2 * 2".
    let b = (
    // MATCH:
    2 * 2
    ) + 3;

    // Matching the outer "*" includes the leading paren of its left operand,
    // so the match starts at "(".
    let c =
    // MATCH:
    (
    2 + 2) * 3;

    // ... and through an index base.
    let e =
    // MATCH:
    (
    x)[0] * 3;
  }
}
