void f(int a) {
  //ERROR: match
  auto l1 = [y = foo(a)](){ use(y); };
  //ERROR: match
  g([y = foo(a)](){ use(y); });
}
