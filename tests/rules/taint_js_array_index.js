// JS has no integer literals: js_to_generic maps every Num to G.Float. A
// constant array index therefore reaches Taint.offset_of_IL as a Float and,
// unless that is recognised as an integer, degrades to the Oany wildcard --
// which makes every constant index alias every other one.

function literalArray() {
  var t = [taint_source(), 1];

  // ruleid: taint-js-array-index
  sink(t[0]);

  // ok: taint-js-array-index
  sink(t[1]);
}

function writeThenRead(arr) {
  arr[0] = taint_source();

  // ruleid: taint-js-array-index
  sink(arr[0]);

  // ok: taint-js-array-index
  sink(arr[1]);
}

// Taint on the whole array (rather than one slot) still reaches every index.
function wholeArray() {
  var t = taint_source();

  // ruleid: taint-js-array-index
  sink(t[3]);
}

// Known gap, and a missing feature rather than a regression. In JS `o[0]` and
// `o["0"]` are the same property, but they lower to Oint 0 and Ostr "0", and
// that equivalence has never been implemented. It used to appear to work only
// because a constant index was the Oany wildcard, so this read matched any key
// at all -- including every unrelated one. Implementing it properly needs a
// `lang` in Taint.offset_of_IL -- see the NOTE there.
function numericStringKey(o) {
  o["0"] = taint_source();

  // todo: taint-js-array-index
  sink(o[0]);
}
