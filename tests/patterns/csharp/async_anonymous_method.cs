class C {
  void M() {
    Func<int, int> a = delegate (int x) { return x; };
    // MATCH:
    Func<int, Task<int>> b = async delegate (int x) { return x; };
  }
}
