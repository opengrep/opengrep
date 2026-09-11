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
