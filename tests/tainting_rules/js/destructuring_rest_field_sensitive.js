// Field-sensitivity for object-rest destructuring (JS shares the TS path).
// When only a specific field is tainted and that field is destructured out,
// the `...rest` binding must NOT carry its taint (rest excludes taken keys).
// When the whole object is tainted, rest stays tainted via its other/unknown
// fields (soundness).

function restExcludesTaintedKey() {
  const obj = {};
  obj.a = source(); // only field a is tainted
  const { a, ...rest } = obj;
  // ruleid: rest-field-sensitive
  sink(a);
  // ok: rest-field-sensitive
  sink(rest);
  // ok: rest-field-sensitive
  sink(rest.a);
}

function restKeepsOtherTaintedField() {
  const obj = {};
  obj.a = source();
  obj.b = source();
  const { a, ...rest } = obj; // rest still holds tainted b
  // ruleid: rest-field-sensitive
  sink(rest);
  // ruleid: rest-field-sensitive
  sink(rest.b);
  // ok: rest-field-sensitive
  sink(rest.a);
}

function multipleTakenKeysCleared() {
  const obj = {};
  obj.a = source();
  obj.b = source();
  const { a, b, ...rest } = obj; // both tainted keys destructured out
  // ok: rest-field-sensitive
  sink(rest);
}
