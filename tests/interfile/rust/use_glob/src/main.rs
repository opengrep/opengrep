mod a;
mod b;

use a::*;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    handle(tainted);
}
