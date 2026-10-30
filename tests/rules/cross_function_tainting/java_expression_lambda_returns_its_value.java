// A lambda whose body is an expression returns the value of the expression.

import java.util.function.Function;

public class ExpressionLambda {
    static String apply(Function<String, String> f, String x) {
        return f.apply(x);
    }

    void identity() {
        String r = apply(x -> x, source());
        // ruleid: test-expression-lambda-returns
        sink(r);
    }

    void constant() {
        String r = apply(x -> "c", source());
        // ok: test-expression-lambda-returns
        sink(r);
    }
}
