class Box<T> {
  // ok: field-type-not-class-type-parameter
  T value;
  // ruleid: field-type-not-class-type-parameter
  int count;
}

class Plain {
  // ruleid: field-type-not-class-type-parameter
  int count;
}
