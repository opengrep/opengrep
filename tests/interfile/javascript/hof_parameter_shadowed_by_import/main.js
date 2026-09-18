import { callback } from "./a";

function propagates(x) { return x; }

function real_hof(callback, value) {
  return callback(value);
}

function test_real_hof() {
  // ruleid: hof-parameter-shadowed-by-import
  sink(real_hof(propagates, source()));
}
