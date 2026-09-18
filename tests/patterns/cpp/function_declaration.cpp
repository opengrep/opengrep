//ERROR: match
void f(int);

void g(int);

void f(int x) {
}

class C {
  //ERROR: match
  void f(int);
};
