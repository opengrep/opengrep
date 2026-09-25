// A constructor that assigns a function to a field of this gives every
// instance that function in the field, which a call through the field reaches.
function g(x) {
  // ruleid: constructor_field_holds_function_javascript
  sink(x);
}

function h(x) {
  // ok: constructor_field_holds_function_javascript
  sink(x);
}

class Holder {
  constructor() {
    this.f = g;
  }
}

class Other {
  constructor() {
    this.f = h;
  }
}

function main() {
  const obj = new Holder();
  obj.f(source());
  const unused = new Other();
}
