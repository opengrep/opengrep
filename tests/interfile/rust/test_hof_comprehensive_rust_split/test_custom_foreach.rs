fn test_custom_foreach() {
    let tainted = source();
    let arr = vec![tainted];
    custom_for_each(&arr, |x| {
        // ruleid: test-hof-taint
        sink(x);
    });
}
