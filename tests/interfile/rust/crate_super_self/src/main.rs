mod a;
mod other;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

pub fn top(x: String) {
    // ruleid: crate-super-self
    sink(&x);
}

fn sink(x: &str) {
    println!("{}", x);
}

fn main() {
    let tainted = source();
    a::b::run(tainted);
}
