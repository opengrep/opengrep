function handler_pos({ body, user }) {
  // ruleid: test-map-destructure-taint
  sink(body);
}
