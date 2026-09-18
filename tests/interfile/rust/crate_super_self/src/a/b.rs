pub fn run(x: String) {
    crate::top(x.clone());
    super::handle(x.clone());
    self::helper(x);
}

pub fn helper(x: String) {
    // ruleid: crate-super-self
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}
