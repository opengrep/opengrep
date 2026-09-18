pub fn handle(x: String) {
    // ok: pub-use-reexport
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
