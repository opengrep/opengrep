// Field-sensitive taint through a JavaScript object destructure.
// The destructure binds `body` and `user` to the same-named keys.
// At the caller, we pass objects where exactly one key carries a
// source; only the sink whose destructured leaf matches the tainted
// key should fire.

function handler_pos({ body, user }) {
  // ruleid: test-map-destructure-taint
  sink(body);
}

function caller_pos() {
  handler_pos({ body: source(), user: "safe" });
}

function handler_neg({ body, user }) {
  // ok: test-map-destructure-taint
  sink(body);
}

function caller_neg() {
  handler_neg({ body: "safe", user: source() });
}
