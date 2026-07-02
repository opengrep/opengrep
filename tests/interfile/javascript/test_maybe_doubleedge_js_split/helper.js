function helper(taintedArg) {
  const a = getInput();
  const b = [];
  taintedArg.method().forEach(elem => b.push(elem));
  return { a, b };
}
