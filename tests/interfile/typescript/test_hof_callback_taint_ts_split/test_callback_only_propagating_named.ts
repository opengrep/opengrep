function test_callback_only_propagating_named(): void {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(propagates, source()));
}
