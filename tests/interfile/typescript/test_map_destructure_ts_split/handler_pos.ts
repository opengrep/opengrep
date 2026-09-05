function handler_pos({ body, user }: { body: string; user: string }) {
  // ruleid: test-map-destructure-taint
  sink(body);
}
