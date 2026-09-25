// A call through a field of an object reaches the functions stored in
// that field of that object, by a literal or by a later assignment.
const M = {
  f(x) {
    // ruleid: object_field_holds_function_javascript
    sink(x);
  },
};

const N = {
  f(x) {
    // ok: object_field_holds_function_javascript
    sink(x);
  },
};

M.g = function (x) {
  // ruleid: object_field_holds_function_javascript
  sink(x);
};

N.g = function (x) {
  // ok: object_field_holds_function_javascript
  sink(x);
};

function g(x) {
  // ruleid: object_field_holds_function_javascript
  sink(x);
}

function h(x) {
  // ok: object_field_holds_function_javascript
  sink(x);
}

const obj = { inner: {} };
const other = { inner: {} };
obj.inner.handler = g;
other.inner.handler = h;

function main() {
  M.f(source());
  M.g(source());
  obj.inner.handler(source());
}
