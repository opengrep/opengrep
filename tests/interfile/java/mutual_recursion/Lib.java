class Lib {
    static boolean cond() {
        return false;
    }

    static Object p(Object x) {
        return q(x);
    }

    static Object q(Object x) {
        return r(source());
    }

    static Object r(Object x) {
        if (cond()) {
            return p(x);
        }
        return x;
    }
}
