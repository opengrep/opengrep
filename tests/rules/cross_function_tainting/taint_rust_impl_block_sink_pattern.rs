struct Foo;

impl Foo {
    fn m(&self) {
        let x = source();
        // ruleid: taint_rust_impl_block_sink_pattern
        sink(x)
    }
}

fn source() -> i32 {
    42
}

fn sink(x: i32) {
    println!("{}", x);
}

fn main() {
    let f = Foo;
    f.m();
}
