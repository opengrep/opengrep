mod lib;

use lib::p;

fn main() {
    // The source is injected inside the cycle p -> q -> r -> p and must
    // come around as p's return value.
    // ruleid: mutual-recursion-rust
    sink(p(0));
}
