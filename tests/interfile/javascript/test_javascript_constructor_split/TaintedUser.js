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
