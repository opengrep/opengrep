function callNestedNo() {
    const y = {outer: {inner: false}};
    nestedNo(y, source());
}
