pub fn send_clean(value: String) {
    let safe = sanitize(value);
    // ok: sanitiser-active-rust
    sink(safe);
}

pub fn send_dirty(value: String) {
    // ruleid: sanitiser-active-rust
    sink(value);
}
