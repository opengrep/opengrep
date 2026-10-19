// OK:
void g() { use(x); }

void f(int x) {
  //ERROR: match
  auto h = [x](){ use(x); };
}
