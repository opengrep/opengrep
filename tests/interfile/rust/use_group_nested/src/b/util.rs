pub fn handle(x: String) {
    // ok: use-group-nested
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
