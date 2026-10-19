class A {
  System.Func<string, string> id = x => x;
  System.Func<string, string> constant = x => "a";
  System.Func<string, System.Text.StringBuilder> builder =
    x => new System.Text.StringBuilder(x);

  static string Apply(System.Func<string, string> f, string x) {
    return f(x);
  }

  void Fields() {
    // ruleid: csharp_expression_lambda
    sink(id(source()));
    // todook: csharp_expression_lambda
    sink(constant(source()));
    // ruleid: csharp_expression_lambda
    sink(builder(source()));
  }

  void Local() {
    System.Func<string> f = () => source();
    // ruleid: csharp_expression_lambda
    sink(f());
    System.Func<string> g = () => { source(); return ""; };
    // ok: csharp_expression_lambda
    sink(g());
  }

  void Argument() {
    // ruleid: csharp_expression_lambda
    sink(Apply(x => x, source()));
    // ok: csharp_expression_lambda
    sink(Apply(x => "a", source()));
  }
}
