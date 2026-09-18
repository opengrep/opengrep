pub fn handle(x: String) {
    // ruleid: callback-function-item
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
