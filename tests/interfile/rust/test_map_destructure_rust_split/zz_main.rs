// Field-sensitive taint through a Rust struct destructure and
// struct literal construction. The destructure binds `body` and
// `user` to the same-named fields; at the caller, a struct literal
// with exactly one tainted field should only flag the handler that
// sinks that field.





