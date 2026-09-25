// A bracket member access with a constant string key selects the same
// member as the dot access with that name.
const obj = {
  m(x) {
    // ruleid: bracket_member_call_javascript
    sink(x);
  },
  n(x) {
    // ok: bracket_member_call_javascript
    sink(x);
  },
};

const other = {
  m(x) {
    // ok: bracket_member_call_javascript
    sink(x);
  },
};

function main() {
  obj["m"](source());
}

const nested = {
  inner: {
    handler(x) {
      // ruleid: bracket_member_call_javascript
      sink(x);
    },
    dotted(x) {
      // ruleid: bracket_member_call_javascript
      sink(x);
    },
    unused(x) {
      // ok: bracket_member_call_javascript
      sink(x);
    },
  },
};

nested.inner["handler"](source());
nested.inner.dotted(source());
