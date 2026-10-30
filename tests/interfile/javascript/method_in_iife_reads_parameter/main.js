// A method defined in an object literal inside an immediately invoked
// function reads a parameter of that function.
(function (input) {
  behaviors.attach = {
    attach() {
      const value = wrap(input);
      // ruleid: method_in_iife_reads_parameter
      sink(value);
    },
    detach() {
      // ok: method_in_iife_reads_parameter
      sink("constant");
    },
  };
})(window.input);
