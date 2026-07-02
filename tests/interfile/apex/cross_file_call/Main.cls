public class Main {
    public static String source() {
        return 'tainted';
    }

    public static void run() {
        String tainted = source();
        Impl.greet(tainted);
    }
}
