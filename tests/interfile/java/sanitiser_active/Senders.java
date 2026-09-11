class Senders {
    static void sendClean(String value) {
        String safe = sanitize(value);
        // ok: sanitiser-active-java
        sink(safe);
    }

    static void sendDirty(String value) {
        // ruleid: sanitiser-active-java
        sink(value);
    }
}
