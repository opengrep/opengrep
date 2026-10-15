class C {
  // A variable of an [out var] declaration, which naming does not resolve,
  // read by a lambda: the lambda captures it.
  void OutVariable() {
    Get(out var o);
    o = source();
    Action g = () => {
      // ruleid: csharp_lambda_reads_unresolved_variable
      sink(o);
    };
    g();
  }

  // The same with a variable of a [for] initialiser.
  void ForInitialiser() {
    for (var i = source(); i < 3; i++) {
      Action g = () => {
        // ruleid: csharp_lambda_reads_unresolved_variable
        sink(i);
      };
      g();
    }
  }

  void UntaintedOutVariable() {
    Get(out var o);
    Action g = () => {
      // ok: csharp_lambda_reads_unresolved_variable
      sink(o);
    };
    g();
  }
}
