function fromObject(data) {
  // ruleid: cjs-named-exports
  sink(data);
}

function fromProperty(data) {
  // ruleid: cjs-named-exports
  sink(data);
}

function notExported(data) {
  // ok: cjs-named-exports
  sink(data);
}

// Object-literal form: the dominant CJS named-export idiom.
module.exports = { fromObject };
// Property form.
module.exports.fromProperty = fromProperty;
