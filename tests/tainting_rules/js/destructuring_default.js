// Object destructuring with a default value must not drop taint.
// JS shares the same frontend/IL-lowering path as TS; this mirrors the
// regression coverage in tests/tainting_rules/ts/destructuring_default.ts.

function shorthandDefault(params) {
  const { a = 0 } = params;
  // ruleid: destructuring-default-taint-loss
  sink(a);
}

function plain(params) {
  const { a } = params; // no default (control)
  // ruleid: destructuring-default-taint-loss
  sink(a);
}

function renamedDefault(params) {
  const { a: b = 0 } = params; // renamed + default (control)
  // ruleid: destructuring-default-taint-loss
  sink(b);
}

function ternary(params) {
  const a = params.a === undefined ? 0 : params.a; // desugared control
  // ruleid: destructuring-default-taint-loss
  sink(a);
}

function nestedDefault(params) {
  const { outer: { inner = 0 } = {} } = params;
  // ruleid: destructuring-default-taint-loss
  sink(inner);
}

// --- Patterns with more than one element ------------------------------------

function twoDefaults(params) {
  const { a = 0, b = 1 } = params;
  // ruleid: destructuring-default-taint-loss
  sink(a);
  // ruleid: destructuring-default-taint-loss
  sink(b);
}

function defaultThenPlain(params) {
  const { a = 0, b } = params;
  // ruleid: destructuring-default-taint-loss
  sink(a);
  // ruleid: destructuring-default-taint-loss
  sink(b);
}

function plainThenDefault(params) {
  const { a, b = 1 } = params;
  // ruleid: destructuring-default-taint-loss
  sink(a);
  // ruleid: destructuring-default-taint-loss
  sink(b);
}

function renamedAndShorthandMixed(params) {
  const { a: x = 0, b, c: z = 2 } = params;
  // ruleid: destructuring-default-taint-loss
  sink(x);
  // ruleid: destructuring-default-taint-loss
  sink(b);
  // ruleid: destructuring-default-taint-loss
  sink(z);
}

function withRest(params) {
  const { a = 0, ...rest } = params;
  // the default binding is tainted even when a rest element follows
  // ruleid: destructuring-default-taint-loss
  sink(a);
  // object-rest bindings drop taint via a separate code path (FieldSpread);
  // that is a distinct pre-existing gap, tracked here as a known TODO.
  // todoruleid: destructuring-default-taint-loss
  sink(rest);
}

function noTaint() {
  const { a = 0 } = {}; // RHS is not a parameter: must stay untainted
  // ok: destructuring-default-taint-loss
  sink(a);
}

function noTaintMulti() {
  const { a = 0, b = 1 } = {}; // multiple defaults, still untainted
  // ok: destructuring-default-taint-loss
  sink(a);
  // ok: destructuring-default-taint-loss
  sink(b);
}
