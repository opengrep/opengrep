using System;

// The expression body of a lambda is its value, whatever the form of the
// expression.
class A {
  string fld;

  static string App(Func<string, string> f, string v) {
    return f(v);
  }

  static object AppFoo(Func<Foo, object> f, Foo v) {
    return f(v);
  }

  void M(string[] arr) {
    // ruleid: csharp_lambda_expression_body_value
    Sink(App(x => fld = x, Source()));
    // ruleid: csharp_lambda_expression_body_value
    Sink(App(x => arr[0] = x, Source()));
    // ruleid: csharp_lambda_expression_body_value
    Sink(App(x => fld += x, Source()));
    // ruleid: csharp_lambda_expression_body_value
    Sink(AppFoo(x => x.n++, Source()));
    // ruleid: csharp_lambda_expression_body_value
    Sink(AppFoo(x => x?.f, Source()));
    // ok: csharp_lambda_expression_body_value
    Sink(App(x => fld = "safe", Source()));
  }
}
