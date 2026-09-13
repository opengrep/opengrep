pub fn handle(x: String) {
    // ok: mod-dir-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
