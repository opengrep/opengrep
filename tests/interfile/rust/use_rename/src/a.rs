pub fn handle(x: String) {
    // ruleid: use-rename
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
