pub fn handle(x: String) {
    // ok: closure-in-variable
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
