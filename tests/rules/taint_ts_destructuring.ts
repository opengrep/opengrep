// TypeScript goes through the same js_to_generic lowering as JavaScript, so
// both the constant-index precision and the destructuring-target binding have
// to hold here too.

function tsLiteralArray(): void {
  const t: any[] = [taint_source(), 1];

  // ruleid: taint-ts-destructuring
  sink(t[0]);

  // ok: taint-ts-destructuring
  sink(t[1]);
}

function tsDeclDestructure(): void {
  const [a, b] = [taint_source(), 1];

  // ruleid: taint-ts-destructuring
  sink(a);

  // ok: taint-ts-destructuring
  sink(b);
}

// Destructuring assignment whose targets are member expressions rather than
// plain names; these reach AST_to_IL through the expr_to_pattern fallback.
function tsMemberTargets(o: any): void {
  [o.x, o.y] = [taint_source(), 1];

  // ruleid: taint-ts-destructuring
  sink(o.x);

  // ok: taint-ts-destructuring
  sink(o.y);
}
