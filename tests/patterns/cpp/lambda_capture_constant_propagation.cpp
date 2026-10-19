void f() {
  //ERROR: match
  auto l1 = [y = 4](){ g(y); };
  //ERROR: match
  auto l2 = [y(4)](){ g(y); };
  //ERROR: match
  auto l3 = [y{4}](){ g(y); };
  int z = 4;
  // OK:
  auto l4 = [z = 5](){ g(z); };
  //ERROR: match
  auto l5 = [z](){ g(z); };
}
