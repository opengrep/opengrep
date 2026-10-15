func f(x: Int, y: Int) {
  //ERROR:
  let a = { [x, y] in use(x, y) }
  //ERROR:
  let b = { [y, x] in use(x, y) }
  // OK:
  let c = { [x] in use(x) }
}
