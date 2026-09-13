pub fn handle(x: String) {
    // ok: use-named-binding
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
