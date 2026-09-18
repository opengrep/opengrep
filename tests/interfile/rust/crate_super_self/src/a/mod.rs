pub mod b;

pub fn handle(x: String) {
    // ruleid: crate-super-self
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
