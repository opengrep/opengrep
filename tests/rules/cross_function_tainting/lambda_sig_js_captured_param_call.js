function tainted(x) {
  const g = () => {
    // ruleid: lambda-sig-js-captured-param-call
    sink(x());
  };
  g();
}
tainted(source());

function clean(x) {
  const g = () => {
    // ok: lambda-sig-js-captured-param-call
    sink(x());
  };
  g();
}
clean("safe");
