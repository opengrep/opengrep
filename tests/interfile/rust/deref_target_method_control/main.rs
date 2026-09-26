struct Inner;

impl Inner {
    fn handle(&self, x: i32) {
        // ruleid: deref-target-method-control
        sink(x);
    }
}

fn run(o: Inner) {
    o.handle(source());
}
