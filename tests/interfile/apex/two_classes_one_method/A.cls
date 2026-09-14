public class A {
    public static void sink(String x) {
        System.debug(x);
    }

    public static void handle(String msg) {
        // ruleid: test-two-classes-one-method
        sink(msg);
    }
}
