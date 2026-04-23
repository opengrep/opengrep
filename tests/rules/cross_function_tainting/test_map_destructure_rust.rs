// Field-sensitive taint through a Rust struct destructure and
// struct literal construction. The destructure binds `body` and
// `user` to the same-named fields; at the caller, a struct literal
// with exactly one tainted field should only flag the handler that
// sinks that field.

struct Req {
    body: String,
    user: String,
}

fn handler_pos(Req { body, user }: Req) {
    // ruleid: test-map-destructure-taint
    sink(body);
}

fn caller_pos() {
    handler_pos(Req { body: source(), user: "safe".to_string() });
}

fn handler_neg(Req { body, user }: Req) {
    // ok: test-map-destructure-taint
    sink(body);
}

fn caller_neg() {
    handler_neg(Req { body: "safe".to_string(), user: source() });
}
