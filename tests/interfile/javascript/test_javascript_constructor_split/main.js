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

    // Test chained method call: new Constructor(tainted).method()
    // ruleid: javascript_constructor_sqli
    sink(new User(source()).getProfile());

    return result;
}
