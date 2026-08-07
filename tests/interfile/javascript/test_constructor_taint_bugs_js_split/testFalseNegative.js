function testFalseNegative() {
    let obj = new InternalSource();
    let result = obj.getData();
    // ruleid: constructor-taint-bugs
    sink(result);
}
