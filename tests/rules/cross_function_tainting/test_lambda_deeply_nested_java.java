// Test: Deeply nested lambdas (3 levels)
class TestLambdaDeeplyNested {

    static void test4() {
        String x = source();
        Runnable level1 = () -> {
            Runnable level2 = () -> {
                Runnable level3 = () -> {
                    // ruleid: test-lambda-deeply-nested-java
                    sink(x);
                };
                level3();
            };
            level2();
        };
        level1();
    }

    // Test: Deeply nested lambdas split across functions
    static void test4_level1(String x) {
        Runnable level2 = () -> {
            Runnable level3 = () -> {
                // ruleid: test-lambda-deeply-nested-java
                sink(x);
            };
            level3();
        };
        level2();
    }

    static void test4_caller() {
        String x = source();
        test4_level1(x);
    }

    static String source() { return "tainted"; }
    static void sink(String x) {}
}
