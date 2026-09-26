package app;

interface I {
    static void sink(String x) { }

    default void handle(String m) {
        // ok: unrelated-defaults-ambiguous
        sink(m);
    }
}

interface J {
    static void sink(String x) { }

    default void handle(String m) {
        // ok: unrelated-defaults-ambiguous
        sink(m);
    }
}

class C implements I, J { }

class Main {
    static String source() { return System.getenv("SECRET"); }

    static void run() {
        new C().handle(source());
    }
}
