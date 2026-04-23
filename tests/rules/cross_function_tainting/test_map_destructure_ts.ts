// Field-sensitive taint through a TypeScript object destructure.
// Same scheme as the JavaScript fixture with an explicit type
// annotation on the destructured parameter.

function handler_pos({ body, user }: { body: string; user: string }) {
  // ruleid: test-map-destructure-taint
  sink(body);
}

function caller_pos() {
  handler_pos({ body: source(), user: "safe" });
}

function handler_neg({ body, user }: { body: string; user: string }) {
  // ok: test-map-destructure-taint
  sink(body);
}

function caller_neg() {
  handler_neg({ body: "safe", user: source() });
}
