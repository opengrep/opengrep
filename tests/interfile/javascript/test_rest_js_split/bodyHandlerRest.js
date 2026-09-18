function bodyHandlerRest(arr) {
  const [head, ...rest] = arr;
  // ruleid: test-rest-js
  sink(rest);
}
