package com.c;

class Caller {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        com.a.Util.run(source());
        com.b.Util.run(source());
    }
}
