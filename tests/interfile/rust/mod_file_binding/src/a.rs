pub fn handle(x: String) {
    // ruleid: mod-file-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
