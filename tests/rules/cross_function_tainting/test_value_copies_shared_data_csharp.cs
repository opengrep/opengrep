using System.Collections.Generic;

struct Inner { public string F; }

struct S {
  public List<string> L;
  public Inner In;
}

class TestValueCopiesSharedData {
  static void AddToList(S s, string v) { s.L.Add(v); }

  static void SetInner(S s, string v) { s.In.F = v; }

  static void Caller() {
    S a = new S();
    AddToList(a, source());
    // ruleid: test-value-copies-shared-data-csharp
    sink(a.L);
    S b = new S();
    SetInner(b, source());
    // ok: test-value-copies-shared-data-csharp
    sink(b.In.F);
  }
}
