// helper() returns { a: getInput(), b }, where b is built via a flow
// the engine cannot follow precisely but which is semantically tainted.
// taintedArg.method() returns a value whose elements inherit
// taintedArg's xtaint via unknown-function propagation; the inline
// forEach lambda mutates b via push, and b carries the resulting taint
// into the returned record. The caller projects .b and sinks it.


