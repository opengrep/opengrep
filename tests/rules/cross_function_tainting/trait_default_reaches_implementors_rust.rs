// A default method of a trait runs for every implementor, and a call on
// self inside it reaches the implementor's own method.
trait Handler {
    fn m(&self, x: String);

    fn run(&self, x: String) {
        self.m(x);
    }
}

struct A {
    v: i32,
}

struct B {
    v: i32,
}

struct C {
    v: i32,
}

impl Handler for A {
    fn m(&self, x: String) {
        // ruleid: trait_default_reaches_implementors_rust
        sink(x);
    }
}

impl Handler for B {
    fn m(&self, x: String) {
        // ruleid: trait_default_reaches_implementors_rust
        sink(x);
    }
}

impl C {
    fn m(&self, x: String) {
        // ok: trait_default_reaches_implementors_rust
        sink(x);
    }
}

fn main() {
    let a = A { v: 0 };
    a.run(source());
    let b = B { v: 0 };
    b.run(source());
}
