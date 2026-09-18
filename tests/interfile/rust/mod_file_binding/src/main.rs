mod a;
mod b;

fn source() -> String {
    std::env::var("SECRET").unwrap_or_default()
}

fn main() {
    let tainted = source();
    a::handle(tainted);
}
