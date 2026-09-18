pub fn handle(x: String) {
    // ruleid: closure-in-variable
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
