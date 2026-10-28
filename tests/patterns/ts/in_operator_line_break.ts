function f(key: string, table: object) {
  // MATCH:
  const found = key
    in table
  const missing = key
    instanceof Object
}
