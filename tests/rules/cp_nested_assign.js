function f(x) {
  return x;
}

// A nested assignment registers the write: the global `a` is assigned
// exactly once, so "v" propagates into its uses in other functions.
b = (a = "v");

function usesA() {
  // ruleid: cp-nested-assign-global
  f(a);
}

// The nested reassignment of `q` also counts as a write, so `q` is no
// longer "assigned just once" and "w" must NOT propagate.
q = "w";

function usesQ() {
  // ok: cp-nested-assign-reassigned
  f(q);
}

r = (q = null);

// An assignment in lvalue position still has a purely-read RHS: V_CONST
// must get its svalue stamped so the pattern literal folds.
V_CONST = "v";

function leak() {
  // ruleid: cp-lvalue-rhs-read
  (y = V_CONST).p = 1;
}
