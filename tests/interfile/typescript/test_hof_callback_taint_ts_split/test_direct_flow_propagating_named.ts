function test_direct_flow_propagating_named(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(propagates, source()));
}
