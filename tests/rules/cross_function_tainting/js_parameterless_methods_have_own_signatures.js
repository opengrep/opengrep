// Two methods without parameters defined in object literals: each is checked
// against its own body.
(function (input) {
  const a = {
    run() {
      // ok: test-parameterless-methods
      sink("constant");
    },
  };
  const b = {
    run() {
      // ruleid: test-parameterless-methods
      sink(input);
    },
  };
})(window.input);
