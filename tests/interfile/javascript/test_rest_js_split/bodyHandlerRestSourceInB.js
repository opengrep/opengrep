function bodyHandlerRestSourceInB(arr) {
  const [a, b, ...rest] = arr;
  // ok: test-rest-js
  sink(rest);
}
