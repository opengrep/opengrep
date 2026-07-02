function handler_neg({ body, user }: { body: string; user: string }) {
  // ok: test-map-destructure-taint
  sink(body);
}
