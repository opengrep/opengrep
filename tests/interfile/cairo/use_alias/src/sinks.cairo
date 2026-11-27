fn sink(x: felt252) -> felt252 {
    x
}

fn relay(x: felt252) -> felt252 {
    // ruleid: test-cairo-use-alias
    sink(x)
}

fn quiet(x: felt252) -> felt252 {
    // ok: test-cairo-use-alias
    sink(x)
}
