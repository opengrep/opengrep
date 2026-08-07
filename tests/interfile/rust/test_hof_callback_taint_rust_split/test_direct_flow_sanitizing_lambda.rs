fn test_direct_flow_sanitizing_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|_x| "3".to_string(), source()));
}
