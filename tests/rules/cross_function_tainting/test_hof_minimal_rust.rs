// Minimal Rust HOF test
fn direct_call<F>(callback: F)
where
    F: Fn(String),
{
    callback(source());
}

fn test_direct_call() {
    direct_call(|x| {
        // ruleid: test-hof-taint
        sink(&x);
    });
}

fn source() -> String {
    "tainted".to_string()
}

fn sink(_s: &String) {}

fn main() {
    test_direct_call();
}
