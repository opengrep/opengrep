function f(n) {
  // MATCH:
  for (using cursor = open(), i = 0; i < n; i++) {
    cursor.next();
  }
  for (let j = 0; j < n; j++) {
    step(j);
  }
}
