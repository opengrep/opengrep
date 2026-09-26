use std::ops::Deref;

struct Inner;

impl Inner {
    fn handle(&self, x: i32) {
        // ruleid: deref-target-method
        sink(x);
    }
}

struct Outer {
    inner: Inner,
}

impl Deref for Outer {
    type Target = Inner;

    fn deref(&self) -> &Inner {
        &self.inner
    }
}

fn run(o: Outer) {
    o.handle(source());
}
