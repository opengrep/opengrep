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
