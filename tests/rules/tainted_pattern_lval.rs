
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

// Elements after a mid-pattern `..` are end-relative, which the positional
// lowering cannot express. They are left unbound (a fixme) rather than bound
// to the wrong slot, so the taint in slot 2 must not leak to either binding.
fn rest_pattern_middle() {
  let [w1, .., w2] = [1, 2, tainted, 3];

  // ok: tainted-pattern-lval
  sink(w1);

  // ok: tainted-pattern-lval
  sink(w2);
}

// Known limitation of the unbound tail: taint genuinely reaching a binding
// after `..` goes undetected.
fn rest_pattern_middle_fn() {
  let [.., z1] = [1, tainted];

  // todoruleid: tainted-pattern-lval
  sink(z1);
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
