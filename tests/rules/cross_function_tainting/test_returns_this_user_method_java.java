// Test: taint flows through a user-defined method that mutates an instance
// field and returns `this`, then a follow-up call reads the field. Companion
// to test_returns_this_java, which covers the same shape for builtin-modelled
// methods (StringBuilder.append).
//
// Currently NOT detected: the signature extractor emits an empty signature
// for `add`, dropping both the side-effect on `this.buf` and the
// `return this`, so taint never crosses the call.

public class TestReturnsThisUserMethodJava {

    private StringBuilder buf = new StringBuilder();

    public TestReturnsThisUserMethodJava add(String x) {
        buf.append(x);
        return this;
    }

    public String build() {
        return buf.toString();
    }

    static void test_return_this_chain_into_variable() {
        TestReturnsThisUserMethodJava cf = new TestReturnsThisUserMethodJava();
        String v = cf.add(source()).build();
        // todoruleid: test-returns-this-user-method-java
        sink(v);
    }

    // Sanity case so the test runner registers the file.
    static void test_direct_flow() {
        // ruleid: test-returns-this-user-method-java
        sink(source());
    }

    static String source() { return "tainted"; }
    static void sink(String x) {}
}
