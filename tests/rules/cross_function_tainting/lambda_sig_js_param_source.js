// A lambda's own (destructured) parameter is a source wherever the lambda is
// analysed.
function go() {
  return load().then(({ data }) => {
    // ruleid: lambda-sig-js-param-source
    sink(data.url);
  });
}
function fixed() {
  return load().then(({ data }) => {
    log(data.url);
    // ok: lambda-sig-js-param-source
    sink("/");
  });
}
