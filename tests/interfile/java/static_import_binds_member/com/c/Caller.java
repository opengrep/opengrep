package com.c;

import static com.a.Util.helper;

class Caller {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        helper(source());
    }
}
