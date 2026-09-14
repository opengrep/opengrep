public class B {
    public static void sink(String x) {
        System.debug(x);
    }

    public static void handle(String msg) {
        // ok: test-two-classes-one-method
        sink(msg);
    }
}
