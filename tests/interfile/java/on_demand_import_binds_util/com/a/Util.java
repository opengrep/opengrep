package com.a;

class Util {
    static void run(String x) {
        // ruleid: on-demand-import-binds-util
        sink(x);
    }

    static void relay(String x) {
        // ok: on-demand-import-binds-util
        sink(x);
    }
}
