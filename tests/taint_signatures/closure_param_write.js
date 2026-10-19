// A closure's write to a field of the enclosing function's parameter is an
// effect of that function, as a write by the function's own body is.
function outer(x) {
  const f = () => {
    x.a = source();
  };
  f();
  sink(x.a);
}
