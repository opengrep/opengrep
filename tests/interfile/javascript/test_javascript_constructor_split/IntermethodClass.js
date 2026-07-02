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
