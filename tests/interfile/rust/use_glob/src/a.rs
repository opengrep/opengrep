pub fn handle(x: String) {
    // ruleid: use-glob
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
