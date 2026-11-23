fn f(x: u8) {
    // MATCH:
    match x {
        ..=5 => a(),
        _ => b(),
    }
    match x {
        ..5 => a(),
        _ => b(),
    }
}
