function bodyHandlerRestSourceInHead(arr) {
  const [head, ...rest] = arr;
  // ok: test-rest-js
  sink(rest);
}
