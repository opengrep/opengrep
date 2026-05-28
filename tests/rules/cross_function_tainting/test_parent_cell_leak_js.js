// helper() returns a literal record { a: <tainted-via-getInput>, b: <untainted> }.
//   - .a's value is the result of getInput() (a tainted source).
//   - .b is built by iterating extern.method() (no source involved); .b's
//     elements have no source-labelled taint at construction.
//
// Previously, .b was dropped from the Obj's Fields map and projection
// of .b in the caller fell through to the parent cell's xtaint
// (carrying .a's source-labelled taint), producing a false positive on
// sink(b). Now .b is recorded as Cell (`Clean, Bot); projection
// returns the explicit Clean and sink(b) does not fire.

function helper() {
  const a = getInput();
  const b = [];
  extern.method().forEach(elem => b.push(elem));
  return { a, b };
}

function caller() {
  const r = helper();
  const { b } = r;
  sink(b);
}
