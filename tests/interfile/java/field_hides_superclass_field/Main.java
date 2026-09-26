package app;

interface Runner {
    void run(String x);
}

class Quiet implements Runner {
    static void sink(String x) { }

    public void run(String x) {
        // ok: field-hides-superclass-field
        sink(x);
    }
}

class Loud implements Runner {
    static void sink(String x) { }

    public void run(String x) {
        // ruleid: field-hides-superclass-field
        sink(x);
    }
}

class Base {
    Quiet runner = new Quiet();
}

class Sub extends Base {
    Loud runner = new Loud();
}

class Main {
    static String source() { return System.getenv("SECRET"); }

    static void run() {
        new Sub().runner.run(source());
    }
}
