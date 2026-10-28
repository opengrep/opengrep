function total(a: number) {
  // MATCH:
  const sum = a + convert<number>(a);
  const other = a + translate(a);
}
