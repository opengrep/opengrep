package com.c;

import com.a.*;

class Caller {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        Util.run(source());
    }
}
