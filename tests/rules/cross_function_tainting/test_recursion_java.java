public class TestRecursion {
    static Object p(Object x) { return q(x); }
    static Object q(Object x) { return r(source()); }
    static Object r(Object x) {
        if (cond()) return p(x);
        return x;
    }

    void test_3cycle() {
        // ruleid: test-recursion-fixpoint
        sink(p(0));
    }

    static Object source() { return null; }
    static void sink(Object x) {}
    static boolean cond() { return false; }
}
