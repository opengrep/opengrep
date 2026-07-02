fn test_direct_flow_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|x| x, source()));
}
