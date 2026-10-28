function f(using: Resource, other: Resource) {
  // MATCH:
  using.close();
  other.close();
}
