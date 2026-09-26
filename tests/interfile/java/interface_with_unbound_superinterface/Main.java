package app;

interface Descriptor extends Comparable<Descriptor> {
    void handle(String x);
}

abstract class AbstractDescriptor implements Descriptor {
    void describe(String x) {
        handle(x);
    }
}

class Loud extends AbstractDescriptor {
    static void sink(String x) { }

    public void handle(String x) {
        // ruleid: interface-with-unbound-superinterface
        sink(x);
    }

    public int compareTo(Descriptor other) { return 0; }
}

class Main {
    static String source() { return System.getenv("SECRET"); }

    static void run() {
        new Loud().describe(source());
    }
}
