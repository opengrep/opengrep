function main() {
  // The source is injected inside the cycle p -> q -> r -> p and must
  // come around as p's return value.
  // ruleid: mutual-recursion-js
  sink(p(0));
}
