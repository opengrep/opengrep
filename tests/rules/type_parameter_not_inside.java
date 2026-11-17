class Box<T> {
  // ok: field-outside-generic-class
  T value;
}

class Plain {
  // ruleid: field-outside-generic-class
  int count;
}
