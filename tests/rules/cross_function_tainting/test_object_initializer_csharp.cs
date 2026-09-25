class K { public string X; public string Y; }

class TestObjectInitializer {
  static void Fields() {
    var o = new K { X = source(), Y = "safe" };
    // ok: test-object-initializer-csharp
    sink(o.Y);
    // ruleid: test-object-initializer-csharp
    sink(o.X);
  }

  static void Collection() {
    var l = new List<string> { source(), "a" };
    // ruleid: test-object-initializer-csharp
    sink(l);
  }

  static void TargetTyped() {
    K t = new() { X = source() };
    // ruleid: test-object-initializer-csharp
    sink(t.X);
  }
}
