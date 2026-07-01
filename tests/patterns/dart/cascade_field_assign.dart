void configure(cfg) {
  // a field assignment inside a cascade is matchable as a normal assignment
  // MATCH:
  cfg..enabled = true..name = 'x';
}
