func f() {
  //ERROR:
  let a = { [y = 4] in g(y) }
  let z = 4
  // OK:
  let b = { [z = 5] in g(z) }
  //ERROR:
  let c = { [z] in g(z) }
}
