func f(x: Int) {
  //ERROR:
  let a = { [y = foo(x)] in use(y) }
  // OK:
  let b = { [y] in use(y) }
}
