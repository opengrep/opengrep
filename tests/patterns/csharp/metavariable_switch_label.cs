class C {
  void f(object k) {
    // ERROR: match
    switch (k) {
      case FOO:
        a();
        break;
    }

    // ERROR: match
    switch (k) {
      case int n:
        a();
        break;
    }

    // ERROR: match
    switch (k) {
      case > 5:
        a();
        break;
    }
  }
}
