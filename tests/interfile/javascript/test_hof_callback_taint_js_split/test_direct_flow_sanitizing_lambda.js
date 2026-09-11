function test_direct_flow_sanitizing_lambda() {
    // ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(x => 3, source()));
}
