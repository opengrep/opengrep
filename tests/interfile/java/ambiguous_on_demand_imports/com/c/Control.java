package com.c;

class Control {
    static String source() {
        return System.getenv("SECRET");
    }

    static void handle(String x) {
        // ruleid: ambiguous-on-demand-imports
        sink(x);
    }

    public static void main(String[] args) {
        handle(source());
    }
}
