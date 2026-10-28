async function f() {
  // MATCH:
  await using
  x = connect();
  await using y = connect();
}
