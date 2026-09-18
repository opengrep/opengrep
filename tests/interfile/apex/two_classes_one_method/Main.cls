public class Main {
    public static String source() {
        return 'tainted';
    }

    public static void run() {
        String t = source();
        A.handle(t);
    }
}
