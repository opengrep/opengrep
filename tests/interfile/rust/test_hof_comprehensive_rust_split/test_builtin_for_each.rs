fn test_builtin_for_each() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter().for_each(|x| {
        // ruleid: test-hof-taint
        sink(x);
    });
}
