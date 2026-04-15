class TestInvokeMethods {

    // Function.apply: nested lambdas invoked via apply
    static void test_apply() {
        String x = source();
        Function outer = (a) -> {
            Function inner = (b) -> {
                // ruleid: test-invoke-methods-java
                sink(b);
                return b;
            };
            return inner.apply(a);
        };
        outer.apply(x);
    }

    // Consumer.accept: nested lambdas invoked via accept
    static void test_accept() {
        String x = source();
        Consumer outer = (a) -> {
            Consumer inner = (b) -> {
                // ruleid: test-invoke-methods-java
                sink(b);
            };
            inner.accept(a);
        };
        outer.accept(x);
    }

    // Runnable.run: nested lambdas capturing tainted variable
    static void test_run() {
        String x = source();
        Runnable outer = () -> {
            Runnable inner = () -> {
                // ruleid: test-invoke-methods-java
                sink(x);
            };
            inner.run();
        };
        outer.run();
    }

    // Callable.call: nested lambdas capturing tainted variable
    static void test_call() {
        String x = source();
        Callable outer = () -> {
            Callable inner = () -> {
                // ruleid: test-invoke-methods-java
                sink(x);
                return x;
            };
            return inner.call();
        };
        outer.call();
    }

    // Negative: no taint source, nested
    static void test_no_taint() {
        String x = "clean";
        Runnable outer = () -> {
            Runnable inner = () -> {
                // ok: test-invoke-methods-java
                sink(x);
            };
            inner.run();
        };
        outer.run();
    }

    static String source() { return "tainted"; }
    static void sink(String x) {}
}
