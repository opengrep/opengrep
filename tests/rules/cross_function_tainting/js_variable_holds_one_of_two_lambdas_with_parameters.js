// A variable that holds one of two lambdas with different parameters holds
// both, whichever is assigned first.
function first(c) {
  var h = function (b) { return 1; };
  if (c) { h = function (a) { return source(); }; }
  // ruleid: js_variable_holds_one_of_two_lambdas_with_parameters
  sink(h(0));
}
function second(c) {
  var h = function (a) { return source(); };
  if (c) { h = function (b) { return 1; }; }
  // ruleid: js_variable_holds_one_of_two_lambdas_with_parameters
  sink(h(0));
}
function clean(c) {
  var h = function (a) { return a; };
  if (c) { h = function (b) { return 1; }; }
  // ok: js_variable_holds_one_of_two_lambdas_with_parameters
  sink(h(0));
}
