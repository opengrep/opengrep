
fn main() {
  let file = tainted;

  // ruleid: tainted-pattern-lval
  sink(file);

  let (file2, arg) = tainted;

  // ruleid: tainted-pattern-lval
  sink(file2);

  // ruleid: tainted-pattern-lval
  sink(arg);

  let file3 : ty = tainted;

  // ruleid: tainted-pattern-lval
  sink(file3)
}

// A constructor pattern is rewritten into a tuple by `pattern`, so it binds
// its arguments just like `let (inner) = tainted` would.
fn constructor_pattern() {
  let Some(inner) = tainted;

  // ruleid: tainted-pattern-lval
  sink(inner);
}

// Record pattern: each field binds through a Dot offset.
fn record_pattern() {
  let P { a, b } = tainted;

  // ruleid: tainted-pattern-lval
  sink(a);

  // ruleid: tainted-pattern-lval
  sink(b);
}

fn list_pattern() {
  let [p, q] = tainted;

  // ruleid: tainted-pattern-lval
  sink(p);

  // ruleid: tainted-pattern-lval
  sink(q);
}

// `x @ pat` binds both the alias and the inner pattern's variables, which is
// why is_destructuring_pattern looks through PatAs.
fn as_pattern() {
  let whole @ (r, s) = tainted;

  // ruleid: tainted-pattern-lval
  sink(whole);

  // ruleid: tainted-pattern-lval
  sink(r);

  // ruleid: tainted-pattern-lval
  sink(s);
}
