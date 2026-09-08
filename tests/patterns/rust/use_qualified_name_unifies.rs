use std::process;

// A name qualified through an import is one binding: the same
// metavariable unifies over both of its uses.
fn main() {
    //ERROR: match
    process::exit(1);
    process::exit(2);
}
