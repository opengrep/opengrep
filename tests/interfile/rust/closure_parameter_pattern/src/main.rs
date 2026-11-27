fn constant_items(p: String) {
    let items = vec![String::from("a")];
    // ok: closure-parameter-pattern
    items.iter().for_each(|e| sink(e));
}

fn tainted_argument() {
    let relay = |e: String| {
        // ruleid: closure-parameter-pattern
        sink(e)
    };
    relay(source());
}

fn main() {
    constant_items(source());
}
