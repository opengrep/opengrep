// Destructuring assignment whose targets are member expressions rather than
// plain names. These reach AST_to_IL through the expr_to_pattern fallback and
// bind to the real lval, so the destructured value actually reaches them.
function memberTargets(o) {
  [o.x, o.y] = [taint_source(), 1];

  // ruleid: taint-js-destructuring-assign
  sink(o.x);

  // Each slot keeps its own index, so the clean slot stays clean.
  // ok: taint-js-destructuring-assign
  sink(o.y);
}

function indexTargets(arr) {
  [arr[0], arr[1]] = taint_source();

  // ruleid: taint-js-destructuring-assign
  sink(arr[0]);
}

// With a literal RHS each slot keeps its own index, so only the target of
// the tainted slot ends up tainted. That needs the constant indices on both
// sides to be precise offsets, which for JS means Float literals have to be
// recognised as integers -- see Taint.offset_of_IL.
function indexTargetsPerSlot(arr) {
  [arr[0], arr[1]] = [taint_source(), 1];

  // ruleid: taint-js-destructuring-assign
  sink(arr[0]);

  // ok: taint-js-destructuring-assign
  sink(arr[1]);
}
