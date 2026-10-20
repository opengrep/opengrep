void f(Object k) {
  // An empty map pattern is not an empty list.
  switch (k) {
    case {}:
      a();
  }

  switch (k) {
    // ERROR: match
    case []:
      a();
  }
}
