public class Impl {
    public static void sink(String x) {
        System.debug(x);
    }

    public static void greet(String msg) {
        // ruleid: test-cross-file-call
        sink(msg);
    }
}
