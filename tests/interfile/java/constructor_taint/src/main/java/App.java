class App {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        String tainted = source();
        Store s = new Store(tainted);
        s.send();
    }
}
