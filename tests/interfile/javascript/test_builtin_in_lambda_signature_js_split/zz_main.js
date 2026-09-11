// The lambda's body uses Array.push to copy its argument into a fresh local
// array, then returns that array. When the lambda is invoked indirectly
// (looked up from a record field or an array index), the engine consults
// the lambda's already-extracted signature rather than re-running the body
// at the call site, so the signature must capture the builtin's effect on
// the local for taint to flow through to the caller's sink.


