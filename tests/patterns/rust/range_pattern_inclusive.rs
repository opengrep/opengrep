fn f(x: u8) {
    // MATCH:
    match x {
        1..=3 => a(),
        _ => b(),
    }
    // MATCH:
    match x {
        1...3 => a(),
        _ => b(),
    }
    match x {
        1..3 => a(),
        _ => b(),
    }
    match x {
        1 | 3 => a(),
        _ => b(),
    }
}
