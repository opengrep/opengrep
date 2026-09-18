package com.b;

class Util {
    static void run(String x) {
        // ok: on-demand-import-binds-util
        sink(x);
    }
}
