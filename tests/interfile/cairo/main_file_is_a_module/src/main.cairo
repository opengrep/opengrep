fn sink(x: felt252) -> felt252 {
    x
}

fn helper(x: felt252) -> felt252 {
    // ruleid: main-file-is-a-module
    sink(x)
}
