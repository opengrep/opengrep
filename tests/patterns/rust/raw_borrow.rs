fn f(y: u8) {
    // MATCH:
    let p = &raw const y;
    let q = &raw mut y;
    let r = &y;
}
