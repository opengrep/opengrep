import java.util.function.Function;

// The expression body of a lambda is its value, whatever the form of the
// expression.
class A {
  String fld;

  static String app(Function<String, String> f, String v) {
    return f.apply(v);
  }

  void m(String[] arr) {
    // ruleid: java_lambda_expression_body_value
    sink(app(x -> fld = x, source()));
    // ruleid: java_lambda_expression_body_value
    sink(app(x -> arr[0] = x, source()));
    // ruleid: java_lambda_expression_body_value
    sink(app(x -> fld += x, source()));
    // ok: java_lambda_expression_body_value
    sink(app(x -> fld = "safe", source()));
  }
}
