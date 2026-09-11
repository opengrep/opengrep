package com.c;

import com.a.Util;

class CallerA {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        Util u = new Util();
        u.run(source());
    }
}
