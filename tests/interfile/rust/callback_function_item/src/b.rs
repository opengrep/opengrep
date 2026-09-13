pub fn handle(x: String) {
    // ok: callback-function-item
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
