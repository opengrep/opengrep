class T {}

[Attr(typeof(T))]
class C<T> {
  T field;
}

class D {
  [Attr(nameof(T))]
  void M<T>(T x) {}
}
