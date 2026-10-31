async function f() {
  // MATCH:
  x = await g<T>(y).z++;
  x = await g<T>(y).w++;
}
