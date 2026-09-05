function bodyHandlerCleanHead(arr) {
  const [head, ...rest] = arr;
  // ok: test-rest-js
  sink(head);
}
