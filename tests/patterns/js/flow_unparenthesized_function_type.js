// @flow
function apply(map: string => number, x: string) {
  // MATCH:
  return transform(map(x));
}
function other(x: string) {
  return keep(x);
}
