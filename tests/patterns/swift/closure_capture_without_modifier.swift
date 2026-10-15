func f(x: Int) {
  //ERROR:
  let a = { [self] in self.go() }
  //ERROR:
  let b = { [weak self] in self?.go() }
  //ERROR:
  let c = { [unowned self] in self.go() }
  // OK:
  let d = { [self, x] in use(x) }
}
