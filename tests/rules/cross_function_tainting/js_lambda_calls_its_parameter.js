// A lambda that calls its parameter, used where the parameter is unknown,
// propagates the arguments of the call, as a call of an unknown function does.
function main() {
  // ruleid: js_lambda_calls_its_parameter
  run((cb) => sink(cb(source())));
  // ok: js_lambda_calls_its_parameter
  run((cb) => sink(cb("safe")));
}
