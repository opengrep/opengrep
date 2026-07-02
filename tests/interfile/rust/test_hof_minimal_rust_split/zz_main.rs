// Minimal Rust HOF test
fn direct_call<F>(callback: F)
where
    F: Fn(String),
{
    callback(source());
}




