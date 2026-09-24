function pick(c) {
  const x = source();
  return c ? () => x : () => 0;
}

function pickRev(c) {
  const x = source();
  return c ? () => 0 : () => x;
}

function pickClean(c) {
  const x = source();
  const y = "clean";
  return c ? () => y : () => 0;
}

function returnedClosures(c) {
  // ruleid: test-closure-join-js
  sink(pick(c)());
  // ruleid: test-closure-join-js
  sink(pickRev(c)());
  // ok: test-closure-join-js
  sink(pickClean(c)());
}

function sameParameters(c) {
  const x = source();
  let f;
  if (c) {
    // ruleid: test-closure-join-js
    f = (a) => sink(a);
  } else {
    f = (a) => console.log(a);
  }
  f(x);
}

function sameParametersRev(c) {
  const x = source();
  let f;
  if (c) {
    f = (a) => console.log(a);
  } else {
    // ruleid: test-closure-join-js
    f = (a) => sink(a);
  }
  f(x);
}

function apply(cb, v) {
  cb(v);
}

function callbackChosenByBranch(c) {
  const x = source();
  let g;
  if (c) {
    g = (a) => console.log(a);
  } else {
    // ruleid: test-closure-join-js
    g = (a) => sink(a);
  }
  apply(g, x);
}
