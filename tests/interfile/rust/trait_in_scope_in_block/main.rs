mod a {
    pub trait Loud {
        fn handle(&self, x: i32) {
            // ruleid: trait-in-scope-in-block
            sink(x);
        }
    }
}

mod b {
    pub trait Quiet {
        fn handle(&self, x: i32) {
            // ok: trait-in-scope-in-block
            sink(x);
        }
    }
}

struct S;

impl a::Loud for S {}
impl b::Quiet for S {}

fn run(s: S) {
    use a::Loud;
    s.handle(source());
}

fn other(s: S) {
    s.handle(source());
}
