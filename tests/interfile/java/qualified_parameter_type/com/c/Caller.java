package com.c;

class Caller {
    static String source() {
        return System.getenv("SECRET");
    }

    static void use(com.a.Store s) {
        s.run(source());
    }
}
