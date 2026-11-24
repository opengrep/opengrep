class CharOrdering {
  void M() {
    var x = source();
    char c = 'A';
    if (c >= 'a') {
      // ok: taint
      sink(x);
    }
    if (c < 'a') {
      // ruleid: taint
      sink(x);
    }
  }
}
