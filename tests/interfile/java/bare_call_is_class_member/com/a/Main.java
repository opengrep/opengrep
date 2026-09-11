package com.a;

class Main {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        Worker w = new Worker();
        w.run(source());
        Other o = new Other();
        o.run(source());
    }
}
