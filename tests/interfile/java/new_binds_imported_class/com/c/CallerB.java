package com.c;

import com.b.Util;

class CallerB {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        Util u = new Util(source());
        u.send();
    }
}
