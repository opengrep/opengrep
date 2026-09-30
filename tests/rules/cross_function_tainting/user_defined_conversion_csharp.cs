class Conv {
    public static implicit operator Conv(int n) {
        return new Conv();
    }
}

class Plain {
    public static implicit operator Target(Plain p) {
        return new Target();
    }

    public static implicit operator string(Plain p) {
        return "";
    }
}

class Target {}

class Wrapped {
    public static implicit operator Wrapped(Plain p) {
        return new Wrapped();
    }
}

class Strict {
    public static explicit operator Strict(int n) {
        return new Strict();
    }
}

class P {
    static string FromInt(Conv c) {
        return source();
    }

    static string InParameterClass(Wrapped w) {
        return source();
    }

    static string InArgumentClass(Target t) {
        return source();
    }

    static string ToBuiltin(string s) {
        return source();
    }

    static string OneOfSeveral(Conv c) {
        return source();
    }

    static string OneOfSeveral(bool b) {
        return "";
    }

    static string ExplicitOnly(Strict s) {
        return source();
    }

    static string ExplicitOnly(long n) {
        return "";
    }

    static string Clean(Conv c) {
        return "";
    }

    static void F() {
        Plain p = new Plain();
        // ruleid: user-defined-conversion
        sink(FromInt(1));
        // ruleid: user-defined-conversion
        sink(InParameterClass(p));
        // ruleid: user-defined-conversion
        sink(InArgumentClass(p));
        // ruleid: user-defined-conversion
        sink(ToBuiltin(p));
        // ruleid: user-defined-conversion
        sink(OneOfSeveral(1));
        // ok: user-defined-conversion
        sink(ExplicitOnly(1));
        // ok: user-defined-conversion
        sink(Clean(1));
    }
}
