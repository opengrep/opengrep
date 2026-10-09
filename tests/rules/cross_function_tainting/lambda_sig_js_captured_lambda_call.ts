// A callback calls a function it reaches through a captured variable: a
// variable holding a lambda, whose result it reads, or an object holding a
// function, whose sink its arguments reach.
function viaVariable() {
  const make = () => source();
  const fixed = () => "x";
  run(() => {
    // ruleid: lambda-sig-js-captured-lambda-call
    sink(make());
    // ok: lambda-sig-js-captured-lambda-call
    sink(fixed());
  });
}

(function () {
  function get(o, k) {
    // ruleid: lambda-sig-js-captured-lambda-call
    sink(k);
  }
  var helpers = { get: get };
  var read = function (o) {
    return helpers.get(o, source());
  };
  read({});
})();
