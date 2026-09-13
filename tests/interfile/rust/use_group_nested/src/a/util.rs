pub fn handle(x: String) {
    // ruleid: use-group-nested
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
