function setup() {
  // MATCH:
  const actual = importOriginal<typeof import("./binding")>();
  const other = importOther<typeof import("./binding")>();
}
