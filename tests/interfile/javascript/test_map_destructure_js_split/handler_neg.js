function handler_neg({ body, user }) {
  // ok: test-map-destructure-taint
  sink(body);
}
