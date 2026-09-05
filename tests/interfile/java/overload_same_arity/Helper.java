class Helper {
    // The safe overload is declared first: the sink one must be found
    // by its parameter type, not by declaration order.
    static void handle(int n) {
        safe(n);
    }

    static void handle(String s) {
        // ruleid: overload-same-arity
        sink(s);
    }
}
