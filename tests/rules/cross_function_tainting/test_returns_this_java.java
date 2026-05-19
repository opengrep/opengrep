// Test: taint flows through the return value of methods modelled with returns_this=true.

public class TestReturnsThisJava {

    // Case 1: return value of append assigned to a variable then passed to sink.
    // This is the primary regression case.
    static void test_append_return_value_tainted() {
        String s = source();
        StringBuilder b = new StringBuilder();
        String v = b.append(s);
        // ruleid: test-returns-this-java
        sink(v);
    }

    // Case 2: return value of append passed directly to sink.
    static void test_append_return_direct() {
        String s = source();
        StringBuilder b = new StringBuilder();
        // ruleid: test-returns-this-java
        sink(b.append(s));
    }

    // Case 3: return value of insert (also returns_this=true) assigned and passed to sink.
    static void test_insert_return_value_tainted() {
        String s = source();
        StringBuilder b = new StringBuilder();
        String v = b.insert(0, s);
        // ruleid: test-returns-this-java
        sink(v);
    }

    // Case 4: pre-existing taint on 'b' should also flow through the return value.
    static void test_append_return_carries_prior_taint() {
        StringBuilder b = new StringBuilder(source());
        String v = b.append("extra");
        // ruleid: test-returns-this-java
        sink(v);
    }

    // Case 5: no taint source - clean data should not reach sink.
    static void test_append_no_taint() {
        StringBuilder b = new StringBuilder();
        String v = b.append("clean");
        // ok: test-returns-this-java
        sink(v);
    }

    static String source() { return "tainted"; }
    static void sink(String x) {}
    static void sink(StringBuilder x) {}
}
