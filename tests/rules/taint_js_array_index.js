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

// In JS `o[0]` and `o["0"]` are the same property: an integer key is
// canonicalized to its decimal string spelling. Taint.offset_of_IL conflates
// a canonical integer spelling into the Oint the numeric form takes -- for
// JS/TS only, since e.g. in Python d[0] and d["0"] really are different
// dict keys.
function numericStringKey(o) {
  o["0"] = taint_source();

  // ruleid: taint-js-array-index
  sink(o[0]);
}

function numericStringKeyOtherDirection(o) {
  o[0] = taint_source();

  // ruleid: taint-js-array-index
  sink(o["0"]);
}

// A non-canonical spelling is a distinct property in JS too: o["00"] and
// o[0] do not alias, nor do o["0x10"] and o[16].
function nonCanonicalStringKey(o) {
  o["00"] = taint_source();

  // ok: taint-js-array-index
  sink(o[0]);
}

// A write through an unknown index lands in the Oany entry of the object
// shape, and a concrete-index read of an untracked field falls back to it
// (Taint_shape.find_in_obj_w_carry) -- the call below passes i = 0, so the
// flow is real. This is the read direction the Oany wildcard used to cover
// before the Float -> Oint change made constant reads concrete.
function unknownIndexWriteConstantRead(arr, i) {
  arr[i] = taint_source();

  // ruleid: taint-js-array-index
  sink(arr[0]);
}

unknownIndexWriteConstantRead([1, 2], 0);
