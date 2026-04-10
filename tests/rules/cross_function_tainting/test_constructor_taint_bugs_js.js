// FALSE NEGATIVE: zero-arg constructor with internal source.
class InternalSource {
    constructor() {
        this.data = source();
    }

    getData() {
        return this.data;
    }
}

// FALSE POSITIVE: constructor ignores its argument.
class IgnoresArg {
    constructor(data) {
        this.data = "safe";
    }

    getData() {
        return this.data;
    }
}

function testFalseNegative() {
    let obj = new InternalSource();
    let result = obj.getData();
    // ruleid: constructor-taint-bugs
    sink(result);
}

function testFalsePositive() {
    let obj = new IgnoresArg(source());
    let result = obj.getData();
    // ok: constructor-taint-bugs
    sink(result);
}
