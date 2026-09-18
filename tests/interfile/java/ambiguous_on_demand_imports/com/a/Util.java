package com.a;

class Util {
    static void run(String x) {
        // ok: ambiguous-on-demand-imports
        sink(x);
    }
}
