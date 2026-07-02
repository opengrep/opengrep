public class Main {
    public static String source() {
        return 'tainted';
    }

    public static void run() {
        String x = source();
        String y = Mid.process(x);
        Impl.emit(y);
    }
}
