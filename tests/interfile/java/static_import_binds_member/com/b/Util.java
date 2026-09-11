package com.b;

class Util {
    public static void helper(String x) {
        // ok: static-import-binds-member
        sink(x);
    }
}
