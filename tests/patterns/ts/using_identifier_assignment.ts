function f(using: Resource | null, other: Resource | null) {
  // MATCH:
  using = null;
  other = null;
}
