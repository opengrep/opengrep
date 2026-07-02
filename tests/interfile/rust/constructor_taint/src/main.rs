mod store;

use store::Store;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    let s = Store { data: tainted };
    s.send();
}
