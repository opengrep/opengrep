async function f() {
  await using
  x = connect();
  // MATCH:
  await (y = connect());
}
