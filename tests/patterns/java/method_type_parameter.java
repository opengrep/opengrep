//ERROR: match
class Box<T> {
  T get(T x) {
    return x;
  }
}

class Outer<T> {
  <T> T shadow(T x) {
    return x;
  }
}
