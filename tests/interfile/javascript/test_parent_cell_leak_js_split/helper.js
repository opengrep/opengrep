function helper() {
  const a = getInput();
  const b = [];
  extern.method().forEach(elem => b.push(elem));
  return { a, b };
}
