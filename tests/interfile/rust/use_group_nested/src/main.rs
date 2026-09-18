mod a;
mod b;

use a::{util::handle, other};

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    other::start();
    handle(tainted);
}
