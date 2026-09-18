package com.example;

class Main {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        String tainted = source();
        Helper h = new Helper();
        h.process(tainted);
    }
}
