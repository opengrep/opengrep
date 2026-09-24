int f(int x) { return x; }

struct A {
  template <typename T> static T g(T x) { return x; }
};

int main() {
  int f = 0;
  ::f(1);
  A::g<int>(2);
  return f;
}
