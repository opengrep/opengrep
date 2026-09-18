const o = {
  run(x) {
    // ruleid: object-literal-method-call
    sink(x);
  }
};

export function go() {
  o.run(source());
}
