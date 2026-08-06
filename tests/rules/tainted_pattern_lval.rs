
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

// A plain alias binds both names even though the inner pattern is a single
// variable, not a destructure.
fn plain_as_pattern() {
  let c @ d = tainted;

  // ruleid: tainted-pattern-lval
  sink(c);

  // ruleid: tainted-pattern-lval
  sink(d);
}

// A trailing `..` skips the remaining elements and binds nothing; the slots
// before it stay exact.
fn rest_pattern_trailing() {
  let [h1, h2, ..] = [tainted, 1, 2, 3];

  // ruleid: tainted-pattern-lval
  sink(h1);

  // ok: tainted-pattern-lval
  sink(h2);
}

// With a literal scrutinee the arity is known, so the rest marker expands
// into exact wildcard slots and a binding after `..` reads its true index:
// w2 is slot 3, not the tainted slot 2.
fn rest_pattern_middle() {
  let [w1, .., w2] = [1, 2, tainted, 3];

  // ok: tainted-pattern-lval
  sink(w1);

  // ok: tainted-pattern-lval
  sink(w2);
}

fn rest_pattern_middle_exact() {
  let [.., z1] = [1, tainted];

  // ruleid: tainted-pattern-lval
  sink(z1);
}

// With an RHS of unknown arity, an element after `..` binds to the Slice
// view of the tail: its taint is the union of every slot it could be — a
// may-over-approximation rather than a wrong slot or a silent drop.
fn rest_pattern_tail_view() {
  let ys = [1, tainted];
  let [.., v1] = ys;

  // ruleid: tainted-pattern-lval
  sink(v1);
}

fn rest_pattern_tail_view_over_approx() {
  let xs = [tainted, 1];
  let [.., v2] = xs;

  // v2 is really slot 1 (clean), but the tail view [0..] includes the
  // tainted slot 0, so this over-approximates and fires.
  // todook: tainted-pattern-lval
  sink(v2);
}

// `..` in a tuple-struct pattern: the leading arguments still bind.
fn rest_pattern_tuple_struct() {
  let P(m1, ..) = tainted;

  // ruleid: tainted-pattern-lval
  sink(m1);
}

// `..` in a record pattern is skipped; the named fields still bind.
fn record_rest_pattern() {
  let Q { fld, .. } = tainted;

  // ruleid: tainted-pattern-lval
  sink(fld);
}
