// Code after loops nested three deep is analysed, also when the loops use up
// the rounds of the fixpoint.
function main() {
  let v0, v1, v2, v3, v4, v5, v6, v7, v8, v9;
  while (c0()) {
    v0 = v1;
    v1 = v2;
    v2 = v3;
    while (c1()) {
      v3 = v4;
      v4 = v5;
      v5 = v6;
      while (c2()) {
        v6 = v7;
        v7 = v8;
        v8 = v9;
        v9 = source();
      }
    }
  }
  let z = source();
  // ruleid: js_code_after_nested_loops
  sink(z);
  // ok: js_code_after_nested_loops
  sink("safe");
}
