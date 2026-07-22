function foo()

  -- A parenthesized operand keeps its own range tight: matching the inner
  -- "*" reports "2 * 2", not "(2 * 2)".
  x = (
  -- MATCH:
  2 * 2
  )

  -- The parens fold into the enclosing "+", not into the inner "*": matching
  -- the "*" still reports the tight "2 * 2".
  x = (
  -- MATCH:
  2 * 2
  ) + 3

  -- Matching the outer "*" includes the leading paren of its left operand,
  -- so the match starts at "(".
  x =
  -- MATCH:
  (
  2 + 2) * 3

  -- ... through a call callee.
  x =
  -- MATCH:
  (
  f)(1) * 3

  -- ... through an index base.
  x =
  -- MATCH:
  (
  t)[1] * 3

  -- ... and through a field-access base.
  x =
  -- MATCH:
  (
  t).k * 3
end
