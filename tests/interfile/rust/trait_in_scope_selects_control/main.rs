mod a {
    pub trait Loud {
        fn handle(&self, x: i32) {
            // ruleid: trait-in-scope-selects-control
            sink(x);
        }
    }
}

struct S;

impl a::Loud for S {}

use a::Loud;

fn run(s: S) {
    s.handle(source());
}
