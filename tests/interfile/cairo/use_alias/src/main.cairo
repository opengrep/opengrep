use crate::sinks::relay as forward;
use crate::sinks::quiet as hush;

fn source() -> felt252 {
    1
}

fn run() -> felt252 {
    forward(source()) + hush(2)
}
