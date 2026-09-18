pub fn handle(x: String) {
    // ok: use-glob
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
