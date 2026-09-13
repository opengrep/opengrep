mod a;

use a::{Left, Right, Store};

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    let store = Store { data: tainted };
    store.emit();
}
