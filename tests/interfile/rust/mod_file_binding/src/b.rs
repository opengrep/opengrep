pub fn handle(x: String) {
    // ok: mod-file-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
