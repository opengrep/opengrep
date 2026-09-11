package com.a;

class Other {
    void run(String data) {
        helper(data);
    }

    private void helper(String x) {
        // ruleid: bare-call-is-class-member
        sink(x);
    }
}
