function writes_captured() {
  let x = "";
  (function () { x = source(); })();
  // ruleid: js-lambda-called-where-defined
  sink(x);
}

function reads_captured() {
  let y = source();
  // ruleid: js-lambda-called-where-defined
  (function () { sink(y); })();
}

function reads_clean_captured() {
  let z = "";
  // ok: js-lambda-called-where-defined
  (function () { sink(z); })();
}
