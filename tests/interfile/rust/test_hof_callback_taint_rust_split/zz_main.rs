// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.



// HOF where taint flows only through callback return
fn app_callback_only<F>(f: F, x: String) -> String
where
    F: Fn(String) -> String,
{
    f(x)
}

// HOF where taint flows through callback AND directly via x
fn app_with_direct_flow<F>(f: F, x: String) -> String
where
    F: Fn(String) -> String,
{
    f(x.clone()) + &x
}

// === Callback-only HOF tests ===





// === Direct flow HOF tests (taint always flows via + x) ===





