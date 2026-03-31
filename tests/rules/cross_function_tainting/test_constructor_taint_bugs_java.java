// FALSE NEGATIVE: zero-arg constructor with internal source.
class InternalSource {
    String data;

    InternalSource() {
        this.data = source();
    }

    String getData() {
        return this.data;
    }
}

// FALSE POSITIVE: constructor ignores its argument.
class IgnoresArg {
    String data;

    IgnoresArg(String input) {
        this.data = "safe";
    }

    String getData() {
        return this.data;
    }
}

class Test {
    void testFalseNegative() {
        InternalSource obj = new InternalSource();
        String result = obj.getData();
        // ruleid: constructor-taint-bugs
        sink(result);
    }

    void testFalsePositive() {
        IgnoresArg obj = new IgnoresArg(source());
        String result = obj.getData();
        // ok: constructor-taint-bugs
        sink(result);
    }
}
