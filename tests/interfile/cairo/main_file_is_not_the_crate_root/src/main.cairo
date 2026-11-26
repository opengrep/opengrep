fn sink(x: felt252) -> felt252 {
    x
}

fn helper(x: felt252) -> felt252 {
    // ok: main-file-is-not-the-crate-root
    sink(x)
}
