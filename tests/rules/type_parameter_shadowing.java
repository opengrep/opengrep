// The strict rule does not match: the two type parameters are distinct
// bindings that share a name.
// ruleid: same-type-parameter-name
class Outer<T> {
  class Inner<T> {
    T value;
  }
}

// ok: same-type-parameter-name
class Other<T> {
  class Inner<U> {
    U value;
  }
}
