class TaintedUser {
    constructor(seller) {
        this.key = source();
    }

    props() {
        // ruleid: javascript_constructor_sqli
        const query = sink(this.key);
        return query;
    }
}

class User {
    constructor(userName) {
        this.name = userName;
    }

    getProfile() {
        // ruleid: javascript_constructor_sqli
        const query = sink(this.name);
        return query;
    }
}

class IntermethodClass {
    taintMethod() {
        return source();
    }

    sinkMethod() {
        // ruleid: javascript_constructor_sqli
        const query = sink(this.taintMethod());
        return query;
    }
}

// Test anonymous arrow function taint flow
const getTainted = () => {
    const y = source();
    return y;
};

function passThrough(z) {
    const w = z;
    return w;
}

function main() {
    const taintedInput = source();

    // Test field assignment taint flow
    const user = new User();
    user.name = taintedInput;
    const result = user.getProfile();

    // Test intermethod taint flow
    const intermethodObj = new IntermethodClass();
    const intermethodResult = intermethodObj.sinkMethod();

    // Test anonymous arrow function
    const x = getTainted();
    const a = passThrough(x);
    // ruleid: javascript_constructor_sqli
    sink(a);

    return result;
}
