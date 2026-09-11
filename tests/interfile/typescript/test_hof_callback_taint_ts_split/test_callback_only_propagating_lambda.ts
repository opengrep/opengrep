function test_callback_only_propagating_lambda(): void {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only((x: string) => x, source()));
}
