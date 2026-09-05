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
