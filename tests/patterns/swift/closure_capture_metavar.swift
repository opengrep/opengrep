func f(x: Int) {
  //ERROR:
  let a = { [x] in use(x) }
  // OK:
  let b = { [y = x] in use(y) }
  // OK:
  let c = { [self] in self.go() }
  // OK:
  let d = { [weak self] in self?.go() }
  // OK:
  let e = { use(x) }
}
