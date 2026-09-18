class Main {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        String tainted = source();
        Dog d = new Dog();
        d.process(tainted);
    }
}
