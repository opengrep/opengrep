mod a;
mod b;

use a::handle as h;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    h(tainted);
}
