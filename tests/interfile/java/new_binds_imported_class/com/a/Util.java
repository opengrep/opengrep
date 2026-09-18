package com.a;

class Util {
    private String data;

    Util(String x) {
        this.data = x;
    }

    void send() {
        // ruleid: new-binds-imported-class
        sink(this.data);
    }
}
