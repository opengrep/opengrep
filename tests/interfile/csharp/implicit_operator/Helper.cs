class Conv {
    public static implicit operator Conv(int n) {
        return new Conv();
    }
}

class Helper {
    public static void Handle(Conv c, string input) {
        // ruleid: implicit-operator
        Sink(input);
    }

    public static void Drop(Conv c, string input) {
        // ok: implicit-operator
        Sink("");
    }

    static void Sink(string x) {}
}
