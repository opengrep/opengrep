// A lambda calls a function declared beside it in the same scope: the
// lambda's argument reaches the helper's sink through the call.
(function () {
  function build(a, b) {
    // ruleid: lambda-sig-js-sibling-function
    sink(a);
    // ok: lambda-sig-js-sibling-function
    sink(b);
  }
  obj.paint = function () {
    build(source(), other);
  };
}());
