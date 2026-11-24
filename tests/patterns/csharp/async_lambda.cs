class C {
  void M() {
    Func<int, int> a = x => x;
    // MATCH:
    Func<int, Task<int>> b = async x => x;
  }
}
