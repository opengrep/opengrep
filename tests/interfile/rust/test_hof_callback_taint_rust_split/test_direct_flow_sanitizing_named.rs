fn test_direct_flow_sanitizing_named() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(sanitizes, source()));
}
