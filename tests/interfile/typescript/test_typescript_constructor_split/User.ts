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
