pub fn handle(x: String) {
    // ruleid: pub-use-reexport
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
