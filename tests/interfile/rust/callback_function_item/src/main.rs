mod a;
mod b;

use a::handle;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn apply<F: Fn(String)>(callback: F, x: String) {
    callback(x);
}

fn main() {
    let tainted = source();
    apply(handle, tainted);
}
