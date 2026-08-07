function main(): void {
    const taintedInput: string = source();
    const user: User = new User(taintedInput);
    const result: string = user.getProfile();

    // Test intermethod taint flow
    const intermethodObj: IntermethodClass = new IntermethodClass();
    const intermethodResult: string = intermethodObj.sinkMethod();

    // Test anonymous arrow function
    const x: string = getTainted();
    const a: string = passThrough(x);
    // ruleid: typescript_constructor_sqli
    const query: string = `SELECT * FROM users WHERE name = ${a}`;

    // Test chained method call: new Constructor(tainted).method()
    // ruleid: typescript_constructor_sqli
    const chainedQuery: string = `SELECT * FROM users WHERE name = ${new User(source()).getProfile()}`;

    return;
}