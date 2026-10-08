// A call on a lambda's own parameter, where the lambda is used without
// arguments, is not a call on a parameter of the enclosing function.
function sinkingFn(x) {
  // ok: lambda-sig-js-own-param-call
  sink(x);
}
function register(cfg, other) {
  app.get('/', (req, res) => res.send(cfg));
}
register(source(), sinkingFn);

function direct(cb) {
  cb(source());
}
function go() {
  direct((v) => {
    // ruleid: lambda-sig-js-own-param-call
    sink(v);
  });
}
