fn test_builtin_map() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter()
        .map(|x| {
            // ruleid: test-hof-taint
            sink(x);
            x
        })
        .collect::<Vec<_>>();
}
