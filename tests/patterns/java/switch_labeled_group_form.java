class C {
  void a(int k) {
    // ERROR: match
    switch (k) {
      case 3: foo(); break;
      default: bar();
    }
  }

  // A switch labeled statement group is not a switch rule.
  void b(int k) {
    switch (k) {
      case 3 -> foo();
      default -> bar();
    }
  }
}
