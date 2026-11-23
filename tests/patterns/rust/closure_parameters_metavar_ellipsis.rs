fn main() {
    // MATCH:
    let a = || foo();
    // MATCH:
    let b = |x| foo(x);
    // MATCH:
    let c = |x: u64, y: u64| foo(x, y);
    let d = foo(1);
}
