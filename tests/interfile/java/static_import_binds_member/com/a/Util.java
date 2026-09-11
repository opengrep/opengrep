package com.a;

class Util {
    public static void helper(String x) {
        // ruleid: static-import-binds-member
        sink(x);
    }
}
