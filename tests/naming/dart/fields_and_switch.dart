class A {
  int fa = 1, fb = 2;
  int g() { return fa + fb; }
}
void f(int x) {
  var a = 1, b = 2;
  foo(a, b);
  switch (x) {
    case 1:
      var c = 1, d = 2;
      foo(c, d);
      break;
  }
}
