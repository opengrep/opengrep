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
