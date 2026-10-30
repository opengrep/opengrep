function apply(f, x) {
  return f(x);
}

function makeSinker() {
  var g = function (y) {
    // ruleid: js_lambda_value_across_functions
    sink(y);
  };
  return g;
}

function passedToHigherOrder() {
  var h = function (z) {
    // ruleid: js_lambda_value_across_functions
    sink(z);
  };
  apply(h, source());
}

function passedToHigherOrderClean() {
  var h = function (z) {
    // ok: js_lambda_value_across_functions
    sink(z);
  };
  apply(h, "safe");
}

function returnedThenCalled() {
  var s = makeSinker();
  s(source());
}

function capturedTaint() {
  var t = source();
  var k = function (unused) {
    // ruleid: js_lambda_value_across_functions
    sink(t);
  };
  apply(k, 1);
}

function capturedClean() {
  var t = "safe";
  var k = function (unused) {
    // ok: js_lambda_value_across_functions
    sink(t);
  };
  apply(k, 1);
}
