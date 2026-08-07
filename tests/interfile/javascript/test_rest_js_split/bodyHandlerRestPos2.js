function bodyHandlerRestPos2(arr) {
  const [a, b, ...rest] = arr;
  // ruleid: test-rest-js
  sink(rest);
}
