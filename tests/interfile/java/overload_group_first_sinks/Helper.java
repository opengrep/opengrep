class Helper {
    static void handle(String s) {
        // ruleid: overload-group-first-sinks
        sink(s);
    }

    static void handle(int n) {
        safe(n);
    }
}
