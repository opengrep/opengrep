function isComponent(x: unknown): x is component {
  // MATCH:
  return check(x);
}
function isOther(x: unknown): x is other {
  return verify(x);
}
