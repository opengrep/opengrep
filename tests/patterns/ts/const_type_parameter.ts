// MATCH:
function first<const T>(xs: T[]) {
  return xs[0];
}
function last<T>(xs: T[]) {
  return xs[xs.length - 1];
}
