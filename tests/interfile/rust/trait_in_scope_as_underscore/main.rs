mod a {
    pub trait Loud {
        fn handle(&self, x: i32) {
            // ruleid: trait-in-scope-as-underscore
            sink(x);
        }
    }
}

mod b {
    pub trait Quiet {
        fn handle(&self, x: i32) {
            // ok: trait-in-scope-as-underscore
            sink(x);
        }
    }
}

struct S;

impl a::Loud for S {}
impl b::Quiet for S {}

use a::Loud as _;

fn run(s: S) {
    s.handle(source());
}
