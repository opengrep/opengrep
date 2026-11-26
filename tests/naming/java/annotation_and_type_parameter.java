class T {}

@Ann(T.class)
class B<T> {
  T y;
}

class D {
  @Ann(T.class)
  <T> void m(T x) {}
}
