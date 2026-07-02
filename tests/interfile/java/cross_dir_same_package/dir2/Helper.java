package com.example;

class Helper {
    void process(String data) {
        // ruleid: test-cross-dir-same-package
        sink(data);
    }

    static void sink(String x) {
        System.out.println(x);
    }
}
