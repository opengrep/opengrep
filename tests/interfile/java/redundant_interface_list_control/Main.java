package app;

interface I {
    static void sink(String x) { }

    default void handle(String m) {
        // ok: redundant-interface-list-control
        sink(m);
    }
}

interface J extends I {
    static void sink(String x) { }

    default void handle(String m) {
        // ruleid: redundant-interface-list-control
        sink(m);
    }
}

class Base { }

class C extends Base implements J { }

class Main {
    static String source() { return System.getenv("SECRET"); }

    static void run() {
        new C().handle(source());
    }
}
