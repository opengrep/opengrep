fn test_direct_call() {
    direct_call(|x| {
        // ruleid: test-hof-taint
        sink(&x);
    });
}
