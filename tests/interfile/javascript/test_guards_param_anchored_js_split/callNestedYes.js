function callNestedYes() {
    const y = {outer: {inner: true}};
    nestedYes(y, source());
}
