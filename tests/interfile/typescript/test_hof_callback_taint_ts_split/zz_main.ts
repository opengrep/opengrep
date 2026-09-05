// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.



// HOF where taint flows only through callback return

// HOF where taint flows through callback AND directly via x

// === Callback-only HOF tests ===





// === Direct flow HOF tests (taint always flows via + x) ===





declare function source(): string;
declare function sink(x: any): void;
