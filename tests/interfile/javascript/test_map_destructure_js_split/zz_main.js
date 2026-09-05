// Field-sensitive taint through a JavaScript object destructure.
// The destructure binds `body` and `user` to the same-named keys.
// At the caller, we pass objects where exactly one key carries a
// source; only the sink whose destructured leaf matches the tainted
// key should fire.




