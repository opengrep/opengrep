class TestParamByReference {
  static void Set(ref string x) { x = source(); }
  static void Get(out string y) { y = source(); }

  static void Caller() {
    string s = "";
    Set(ref s);
    // ruleid: test-param-by-reference-csharp
    sink(s);
    string t;
    Get(out t);
    // ruleid: test-param-by-reference-csharp
    sink(t);
  }
}
