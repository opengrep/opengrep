pub fn handle(x: String) {
    // ruleid: mod-dir-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
