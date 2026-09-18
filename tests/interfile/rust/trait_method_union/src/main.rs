mod a;
mod b;

use a::{Sink, Store};

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    let store = Store { data: tainted };
    store.emit();
}
