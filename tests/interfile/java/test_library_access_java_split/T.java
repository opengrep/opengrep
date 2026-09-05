class T {
    static void handlerGetPos() {
        Map<String, String> m = new HashMap<>();
        m.put("body", source());
        m.put("user", "safe");
        // ruleid: test-library-access-taint
        sink(m.get("body"));
    }

    static void handlerGetNeg() {
        Map<String, String> m = new HashMap<>();
        m.put("body", "safe");
        m.put("user", source());
        // ok: test-library-access-taint
        sink(m.get("body"));
    }

    static void handlerGetOrDefaultPos() {
        Map<String, String> m = new HashMap<>();
        m.put("body", source());
        // ruleid: test-library-access-taint
        sink(m.getOrDefault("body", "fallback"));
    }

    static void handlerGetOrDefaultTaintedDefault() {
        Map<String, String> m = new HashMap<>();
        m.put("body", "safe");
        // ruleid: test-library-access-taint
        sink(m.getOrDefault("body", source()));
    }
}
