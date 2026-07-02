// HOF guard rebinding across a JavaScript call chain. Inner's
// [ToSinkInCall] (the callback invocation) is guarded by a branch
// condition on one of its parameters. Outer forwards both the callback
// and the guard-relevant parameter to inner. At top-level
// instantiation, the guard should drop the effect when the
// top-level-caller argument fails the condition.





// ---------- No finding: top-level opts.flag is false ----------






// ---------- Finding expected: top-level opts.flag is true ----------




