mod a;
mod b;

use crate::a::handle;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    handle(tainted);
}
