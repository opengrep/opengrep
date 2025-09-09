class TaintedUser {
    private key: string;

    constructor(seller: string) {
        this.key = source();
    }

    props(): void {
        // ruleid: typescript_constructor_sqli
        const query: string = `SELECT * FROM table WHERE name = ${this.key}`;
        return;
    }
}

class User {
    private name: string;

    constructor(userName: string) {
        this.name = userName;
    }

    getProfile(): string {
        // ruleid: typescript_constructor_sqli
        const query: string = `SELECT * FROM users WHERE name = ${this.name}`;
        return query;
    }
}

class IntermethodClass {
    taintMethod(): string {
        return source();
    }

    sinkMethod(): string {
        // ruleid: typescript_constructor_sqli
        const query: string = `SELECT * FROM users WHERE name = ${this.taintMethod()}`;
        return query;
    }
}

// Test anonymous arrow function taint flow
const getTainted = (): string => {
    const y: string = source();
    return y;
};

function passThrough(z: string): string {
    const w: string = z;
    return w;
}

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

    return;
}