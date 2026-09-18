pub fn helper(x: String) {
    // ok: crate-super-self
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
