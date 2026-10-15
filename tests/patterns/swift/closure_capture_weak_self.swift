func f() {
  //ERROR:
  let a = { [weak self] in self?.go() }
  // OK:
  let b = { [self] in self.go() }
  // OK:
  let c = { [unowned self] in self.go() }
}
