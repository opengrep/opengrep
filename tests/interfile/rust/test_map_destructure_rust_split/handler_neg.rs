fn handler_neg(Req { body, user }: Req) {
    // ok: test-map-destructure-taint
    sink(body);
}
