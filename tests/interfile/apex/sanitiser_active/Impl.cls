public class Impl {
    public static void sink(String x) {
        System.debug(x);
    }

    public static void emit(String data) {
        // ok: test-sanitiser
        sink(data);
    }
}
