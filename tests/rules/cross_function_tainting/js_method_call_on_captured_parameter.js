// A method call on a parameter captured by nested lambdas propagates the
// parameter's taint, as an unknown call on the parameter itself does.
module.exports = (filePath) => {
  compile(filePath, function check(code) {
    const name = filePath.slice(0, -9);
    const fixed = "styles".slice(0, -1);
    read(name, function done(err, data) {
      // ruleid: js_method_call_on_captured_parameter
      sink(name);
      // ok: js_method_call_on_captured_parameter
      sink(fixed);
    });
  });
};
