import java.util.function.Function;

// An expression-bodied lambda returns its expression.
class ExprBody {
  String wrap(String s) {
    return "[" + s + "]";
  }

  void f() {
    Function<String, String> g = x -> wrap(x);
    // ruleid: lambda-sig-java-expr-body
    sink(g.apply(source()));
    Function<String, String> h = x -> { return wrap(x); };
    // ruleid: lambda-sig-java-expr-body
    sink(h.apply(source()));
    Function<String, String> k = x -> "[]";
    // ok: lambda-sig-java-expr-body
    sink(k.apply(source()));
  }
}
