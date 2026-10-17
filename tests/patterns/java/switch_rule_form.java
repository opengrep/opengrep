class C {
  void a(int k) {
    // ERROR: match
    switch (k) {
      case 1 -> foo();
      default -> bar();
    }
  }

  // A switch rule is not a switch labeled statement group.
  void b(int k) {
    switch (k) {
      case 1:
        foo();
      default:
        bar();
    }
  }
}
