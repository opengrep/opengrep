function testFalsePositive() {
    let obj = new IgnoresArg(source());
    let result = obj.getData();
    // ok: constructor-taint-bugs
    sink(result);
}
