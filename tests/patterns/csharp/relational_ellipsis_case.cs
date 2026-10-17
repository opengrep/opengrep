class C {
  void f(int k) {
    // ERROR: match
    switch (k) {
      case < 5:
        a();
        break;
    }

    switch (k) {
      case > 5:
        a();
        break;
    }

    switch (k) {
      case 7:
        a();
        break;
    }

    switch (k) {
      default:
        a();
        break;
    }
  }
}
