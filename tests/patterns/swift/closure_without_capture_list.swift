func f() {
  //ERROR:
  let a = { self.go() }
  //ERROR:
  let b = { [weak self] in self.go() }
}
