public class Impl {
    public static void sink(String x) {
        System.debug(x);
    }

    public static void leak(String x) {
        // ruleid: test-passthrough
        sink(x);
    }
}
