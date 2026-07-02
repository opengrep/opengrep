fn test_builtin_filter() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter()
        .filter(|x| {
            // ruleid: test-hof-taint
            sink(x);
            true
        })
        .collect::<Vec<_>>();
}
