const handlers = {
  ping() {
    return 1;
  },
};

class A {
  constant_reaches_sink(c) {
    let x = "secret";
    if (c) {
      x = "secret";
    }
    // ruleid: cp_method_after_object_literal_method_js
    sink(x);
  }

  other_value_reaches_sink(c) {
    let x = "public";
    if (c) {
      x = "public";
    }
    // ok: cp_method_after_object_literal_method_js
    sink(x);
  }
}
