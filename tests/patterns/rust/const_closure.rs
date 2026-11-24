fn main() {
    // MATCH:
    let a = const |x: u8| x + 1;
    let b = |x: u8| x + 1;
}
