func f(x: Int) {
  //ERROR:
  let a = { [weak self] in self?.go() }
  // OK:
  let b = { [x, weak self] in self?.go() }
  // OK:
  let c = { [x] in use(x) }
}
