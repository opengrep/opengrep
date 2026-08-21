fn handler_pos(Req { body, user }: Req) {
    // ruleid: test-map-destructure-taint
    sink(body);
}
