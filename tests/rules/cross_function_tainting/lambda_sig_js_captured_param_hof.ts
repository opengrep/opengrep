// A callback passed to a built-in captures a parameter of the lambda forming
// it: applied by the built-in, that parameter is still the enclosing lambda's,
// not the callback's own first argument.
function f(rows) {
  const keys = rows.map((r) => source(r));
  return keys.map((key, i) => {
    return rows.map((row) => {
      // ruleid: lambda-sig-js-captured-param-hof
      sink(key);
      // ok: lambda-sig-js-captured-param-hof
      sink(row[i]);
    });
  });
}
