fn test_custom_map() {
    let tainted = source();
    let arr = vec![tainted];
    custom_map(&arr, |x| {
        // ruleid: test-hof-taint
        sink(x);
        x.clone()
    });
}
