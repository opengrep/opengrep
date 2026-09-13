mod a;
mod b;

use a::handle;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    let forward = |x: String| handle(x);
    forward(tainted);
}
