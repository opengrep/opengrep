pub fn handle(x: String) {
    // ruleid: use-named-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
