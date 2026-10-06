// A callback passed to a built-in captures a parameter of the lambda forming
// it: applied by the built-in, that parameter is still the enclosing lambda's,
// not the callback's own first argument.
function strs(values) {
  return values.map((v) => JSON.stringify(v));
}
function f(frames) {
  return frames.map((frame) => {
    const first = frame.fields[0];
    const headers = ['x', ...strs(first.values)];
    const newFields = headers.map((fieldName, index) => {
      if (index === 0) {
        return { name: 'n', values: [] };
      }
      const values = frame.fields.map((field) => {
        if (first.type === 1) {
          // ruleid: lambda-sig-js-captured-param-hof
          return strs([field.values[index - 1]])[0];
        }
        // ok: lambda-sig-js-captured-param-hof
        return field.values[index - 1];
      });
      return { name: fieldName, values };
    });
    return newFields;
  });
}
