export function first(x) {
  // ok: last-binding-in-scope-wins
  sink(x);
}
