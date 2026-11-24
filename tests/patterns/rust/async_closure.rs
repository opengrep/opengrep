fn main() {
    // MATCH:
    let a = async |x: u64| foo(x);
    let b = |x: u64| foo(x);
}
