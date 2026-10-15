void f(int a) {
  //ERROR: match
  g([y = foo(a)](){ use(y); });
  // OK:
  g([y = a](){ use(y); });
}
