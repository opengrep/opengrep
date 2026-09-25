struct S { public string F; }
class K { public string F; }

class TestValueCopies {
  static void ByStruct(S s) { s.F = source(); }
  static void ByRefStruct(ref S s) { s.F = source(); }
  static void ByClass(K k) { k.F = source(); }

  static void Caller() {
    S a = new S();
    ByStruct(a);
    // ok: test-value-copies-csharp
    sink(a.F);
    S c = new S();
    ByRefStruct(ref c);
    // ruleid: test-value-copies-csharp
    sink(c.F);
    K b = new K();
    ByClass(b);
    // ruleid: test-value-copies-csharp
    sink(b.F);
  }
}
