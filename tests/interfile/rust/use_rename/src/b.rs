pub fn handle(x: String) {
    // ok: use-rename
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
