use crate::sinks::relay;
use crate::sinks::quiet;

fn source() -> felt252 {
    1
}

fn run() -> felt252 {
    relay(source()) + quiet(2)
}
